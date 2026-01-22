;;; llm-ollama.el --- llm module for integrating with Ollama. -*- lexical-binding: t; package-lint-main-file: "llm.el"; byte-compile-docstring-max-column: 200-*-

;; Copyright (c) 2023-2025  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file implements the llm functionality defined in llm.el, for Ollama, an
;; interface to running LLMs locally.  Ollama can be found at https://ollama.ai/.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-provider-utils)
(require 'llm-models)
(require 'plz)
(require 'json)
(require 'plz-media-type)

(defgroup llm-ollama nil
  "LLM implementation for Ollama."
  :group 'llm)

(defcustom llm-ollama-example-prelude "Examples of how you should respond follow."
  "The prelude to use for examples in Ollama chat prompts."
  :type 'string
  :group 'llm-ollama)

(defcustom llm-ollama-chat-timeout 300
  "Timeout for sync ollama chat calls."
  :type 'integer
  :group 'llm-ollama)

(cl-defstruct (llm-ollama (:include llm-standard-full-provider))
  "A structure for holding information needed by Ollama's API.

SCHEME is the http scheme to use, a string.  It is optional and
default to `http'.

HOST is the host that Ollama is running on.  It is optional and
default to localhost.

PORT is the localhost port that Ollama is running on.  It is optional.

CHAT-MODEL is the model to use for chat queries.  It is required.

EMBEDDING-MODEL is the model to use for embeddings.  It is required."
  (scheme "http") (host "localhost") (port 11434) chat-model embedding-model)

(cl-defstruct (llm-ollama-authed (:include llm-ollama))
  "Similar to llm-ollama, but also with a key."
  key)

(cl-defmethod llm-provider-headers ((provider llm-ollama-authed))
  `(("Authorization" . ,(format "Bearer %s" (encode-coding-string (llm-ollama-authed-key provider) 'utf-8)))))

;; Ollama's models may or may not be free, we have no way of knowing.  There's no
;; way to tell, and no ToS to point out here.
(cl-defmethod llm-nonfree-message-info ((provider llm-ollama))
  (ignore provider)
  nil)

(defun llm-ollama--url (provider method)
  "With ollama PROVIDER, return url for METHOD."
  (format "%s://%s:%d/api/%s" (llm-ollama-scheme provider )(llm-ollama-host provider)
          (llm-ollama-port provider) method))

(cl-defmethod llm-provider-embedding-url ((provider llm-ollama) &optional _)
  (llm-ollama--url provider "embed"))

(cl-defmethod llm-provider-chat-url ((provider llm-ollama))
  (llm-ollama--url provider "chat"))

(cl-defmethod llm-provider-chat-timeout ((_ llm-ollama))
  llm-ollama-chat-timeout)

(cl-defmethod llm-provider-embedding-extract-error ((_ llm-ollama) response)
  (assoc-default 'error response))

(cl-defmethod llm-provider-chat-extract-error ((_ llm-ollama) response)
  (assoc-default 'error response))

(cl-defmethod llm-provider-embedding-request ((provider llm-ollama) string)
  "Return the request to the server for the embedding of STRING.
PROVIDER is the llm-ollama provider."
  `(:input ,string
           :model ,(llm-ollama-embedding-model provider)))

(cl-defmethod llm-provider-batch-embeddings-request ((provider llm-ollama) strings)
  (llm-provider-embedding-request provider (apply #'vector strings)))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-ollama) response)
  "Return the embedding from the server RESPONSE."
  (aref (assoc-default 'embeddings response) 0))

(cl-defmethod llm-provider-batch-embeddings-extract-result ((_ llm-ollama) response)
  (append (assoc-default 'embeddings response) nil))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-ollama) response)
  "Return the chat response from the server RESPONSE."
  (assoc-default 'content (assoc-default 'message response)))

(cl-defmethod llm-provider-extract-reasoning ((_ llm-ollama) response)
  (assoc-default 'thinking (assoc-default 'message response)))

(defun llm-ollama--response-format (format)
  "Return the response format for FORMAT."
  (if (eq format 'json)
      :json
    (llm-provider-utils-convert-to-serializable format)))

(cl-defmethod llm-provider-chat-request ((provider llm-ollama) prompt streaming)
  (llm-provider-utils-combine-to-system-prompt prompt llm-ollama-example-prelude)
  (let (request-plist messages options)
    (setq messages
          (vconcat (mapcan (lambda (interaction)
                             (let* ((role (llm-chat-prompt-interaction-role interaction))
                                    (content (llm-chat-prompt-interaction-content interaction))
                                    (tool-results (llm-chat-prompt-interaction-tool-results interaction))
                                    (tool-call-p (and (listp content)
                                                      (llm-provider-utils-tool-use-p (car content))))
                                    (images nil))
                               ;; Tool results expand to one tool line per
                               ;; result, so multiple lines per this
                               ;; interaction.
                               (if tool-results
                                   (mapcar (lambda (r)
                                             `(:role "tool"
                                                     :tool_name ,(llm-chat-prompt-tool-result-tool-name r)
                                                     :content ,(llm-chat-prompt-tool-result-result r)))
                                           tool-results)
                                 (list (append
                                        `(:role
                                          ,(symbol-name role)
                                          ,(if tool-call-p :tool_calls :content)
                                          ,(cond
                                            ((stringp content) content)
                                            (tool-call-p
                                             (cl-loop for tool in content
                                                      and index from 0
                                                      vconcat
                                                      `((:type "function"
                                                               :function
                                                               (:index ,index
                                                                       :name
                                                                       ,(llm-provider-utils-tool-use-name tool)
                                                                       :arguments
                                                                       ,(llm-provider-utils-tool-use-args tool))))))
                                            ((llm-multipart-p content)
                                             (cl-loop for part in (llm-multipart-parts content) do
                                                      (when (llm-media-p part)
                                                        (setq images (append images (list part))))
                                                      concat
                                                      (if (llm-media-p part)
                                                          ""
                                                        part)))
                                            (t (json-serialize content))))
                                        (when images
                                          `(:images
                                            ,(vconcat (mapcar (lambda (img) (base64-encode-string (llm-media-data img) t))
                                                              images)))))))))
                           (llm-chat-prompt-interactions prompt))))
    (setq request-plist (plist-put request-plist :messages messages))
    (setq request-plist (plist-put request-plist :model (llm-ollama-chat-model provider)))
    (when (llm-chat-prompt-tools prompt)
      (setq request-plist (plist-put
                           request-plist :tools
                           (vconcat (mapcar #'llm-provider-utils-openai-tool-spec
                                            (llm-chat-prompt-tools prompt))))))
    (when (llm-chat-prompt-response-format prompt)
      (setq request-plist (plist-put request-plist :format
                                     (llm-ollama--response-format
                                      (llm-chat-prompt-response-format prompt)))))
    (setq request-plist (plist-put request-plist :stream (if streaming t :false)))
    (let ((model (llm-models-match (llm-ollama-chat-model provider))))
      (when (and (llm-chat-prompt-reasoning prompt)
                 (member 'reasoning (llm-model-capabilities model)))
        (setq request-plist (plist-put request-plist :think
                                       (if (eq 'gpt-oss (llm-model-symbol model))
                                           (pcase (llm-chat-prompt-reasoning prompt)
                                             ('light "low")
                                             ('medium "medium")
                                             ('maximum "high")
                                             ('none :false))
                                         (if (eq 'none (llm-chat-prompt-reasoning prompt))
                                             :false
                                           't))))))
    (when (llm-chat-prompt-temperature prompt)
      (setq options (plist-put options :temperature (llm-chat-prompt-temperature prompt))))
    (when (llm-chat-prompt-max-tokens prompt)
      (setq options (plist-put options :num_predict (llm-chat-prompt-max-tokens prompt))))
    (when-let* ((more-options-plist (llm-provider-utils-non-standard-params-plist prompt)))
      (when-let* ((keep-alive (plist-get more-options-plist :keep_alive)))
        (setq request-plist (plist-put request-plist :keep_alive keep-alive)))
      (setq options (append options
                            (map-into (map-filter (lambda (key _) (not (equal key :keep_alive)))
                                                  more-options-plist)
                                      'plist))))
    (when options
      (setq request-plist (plist-put request-plist :options options)))
    request-plist))

(cl-defmethod llm-provider-extract-tool-uses ((_ llm-ollama) response)
  (mapcar (lambda (call)
            (let ((function (cdadr call)))
              (make-llm-provider-utils-tool-use
               :name (assoc-default 'name function)
               :args (assoc-default 'arguments function))))
          (assoc-default 'tool_calls (assoc-default 'message response))))

(cl-defmethod llm-provider-populate-tool-uses ((_ llm-ollama) prompt tool-uses)
  (llm-provider-utils-append-to-prompt prompt tool-uses))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-ollama) receiver _)
  (cons 'application/x-ndjson
        (plz-media-type:application/x-ndjson
         :handler (lambda (data)
                    (let* ((message (assoc-default 'message data))
                           (text (assoc-default 'content message))
                           (reasoning (assoc-default 'thinking message))
                           (tool-call (assoc-default 'tool_calls message))
                           (response nil))
                      (when reasoning
                        (setq response
                              (plist-put response :reasoning
                                         (concat
                                          (or (plist-get response :reasoning) "")
                                          reasoning))))
                      (when text
                        (setq response
                              (plist-put response :text (concat
                                                         (or (plist-get response :text) "")
                                                         text))))
                      (when tool-call
                        (setq response
                              (plist-put response :tool-uses-raw
                                         (aref tool-call 0))))
                      (funcall receiver response))))))

(cl-defmethod llm-provider-collect-streaming-tool-uses ((_ llm-ollama) data)
  ;; Ollama only supports one tool used at a time.
  (when-let* ((f-alist (cdadr data)))
    (list (make-llm-provider-utils-tool-use
           :name (assoc-default 'name f-alist)
           :args (assoc-default 'arguments f-alist)))))

(cl-defmethod llm-name ((provider llm-ollama))
  (or (llm-ollama-chat-model provider)
      (llm-ollama-embedding-model provider)))

(cl-defmethod llm-chat-token-limit ((provider llm-ollama))
  (llm-provider-utils-model-token-limit (llm-ollama-chat-model provider)
                                        2048))

(cl-defmethod llm-capabilities ((provider llm-ollama))
  (append '(streaming json-response model-list)
          (when (and (llm-ollama-embedding-model provider)
                     (let ((embedding-model (llm-models-match
                                             (llm-ollama-embedding-model provider))))
                       (and embedding-model
                            (member 'embedding (llm-model-capabilities embedding-model)))))
            '(embeddings embeddings-batch))
          (when-let* ((model (llm-ollama-chat-model provider))
                      (chat-model (llm-models-match model))
                      (capabilities (llm-model-capabilities chat-model)))
            (append
             (when (member 'tool-use capabilities) '(tool-use streaming-tool-use))
             (seq-intersection capabilities '(image-input))))))

(cl-defmethod llm-models ((provider llm-ollama))
  (mapcar (lambda (model-data)
            (plist-get model-data :name))
          (plist-get (plz 'get (llm-ollama--url provider "tags")
                       :as (lambda ()
                             (json-parse-buffer :object-type 'plist)))
                     :models )))

(provide 'llm-ollama)

;;; llm-ollama.el ends here
