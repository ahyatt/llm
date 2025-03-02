;;; llm-ollama.el --- llm module for integrating with Ollama. -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
  (llm-provider-embedding-request provider strings))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-ollama) response)
  "Return the embedding from the server RESPONSE."
  (aref (assoc-default 'embeddings response) 0))

(cl-defmethod llm-provider-batch-embeddings-extract-result ((_ llm-ollama) response)
  (append (assoc-default 'embeddings response) nil))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-ollama) response)
  "Return the chat response from the server RESPONSE."
  (let ((raw-result (assoc-default 'content (assoc-default 'message response))))
    ;; The raw result may have reasoning content in, which is in <think> tags
    ;; (for DeepSeek reasoning).  We want to strip that out.
    (with-temp-buffer
      (insert raw-result)
      (goto-char 0)
      (if (search-forward "\n</think>" nil t)
          (string-trim (buffer-substring (point) (point-max)))
        raw-result))))

(cl-defmethod llm-provider-extract-reasoning ((_ llm-ollama) response)
  (let ((raw-result (assoc-default 'content (assoc-default 'message response))))
    ;; Reasoning content is in <think> tags (for DeepSeek reasoning).  We want to
    ;; extract the content between these tags.
    (with-temp-buffer
      (insert raw-result)
      (goto-char 0)
      (when (search-forward "<think>\n" nil t)
        (let* ((endtag "\n</think>")
               (end (save-excursion
                      (search-forward endtag))))
          (buffer-substring (point) (- end (length endtag))))))))

(defun llm-ollama--response-format (format)
  "Return the response format for FORMAT."
  (if (eq format 'json)
      :json
    (llm-provider-utils-convert-to-serializable format)))

(cl-defmethod llm-provider-chat-request ((provider llm-ollama) prompt streaming)
  (llm-provider-utils-combine-to-system-prompt prompt llm-ollama-example-prelude)
  (let (request-plist messages options)
    (setq messages
          (vconcat (mapcar (lambda (interaction)
                             (let* ((role (llm-chat-prompt-interaction-role interaction))
                                    (content (llm-chat-prompt-interaction-content interaction))
                                    (content-text "")
                                    (images nil))
                               (if (stringp content)
                                   (setq content-text content)
                                 (if (eq 'user role)
                                     (dolist (part (llm-multipart-parts content))
                                       (if (llm-media-p part)
                                           (setq images (append images (list part)))
                                         (setq content-text (concat content-text part))))
                                   (setq content-text (json-serialize content))))
                               (append
                                `(:role ,(symbol-name role)
                                        :content ,content-text)
                                (when images
                                  `(:images
                                    ,(vconcat (mapcar (lambda (img) (base64-encode-string (llm-media-data img) t))
                                                      images)))))))
                           (llm-chat-prompt-interactions prompt))))
    (setq request-plist (plist-put request-plist :messages messages))
    (setq request-plist (plist-put request-plist :model (llm-ollama-chat-model provider)))
    (when (and streaming (llm-chat-prompt-tools prompt))
      (signal 'not-implemented
              "Ollama does not support streaming with function calls"))
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
    (when (llm-chat-prompt-temperature prompt)
      (setq options (plist-put options :temperature (llm-chat-prompt-temperature prompt))))
    (when (llm-chat-prompt-max-tokens prompt)
      (setq options (plist-put options :num_predict (llm-chat-prompt-max-tokens prompt))))
    (setq options (append options (llm-provider-utils-non-standard-params-plist prompt)))
    (when options
      (setq request-plist (plist-put request-plist :options options)))
    request-plist))

(cl-defmethod llm-provider-extract-tool-uses ((_ llm-ollama) response)
  (mapcar (lambda (call)
            (let ((function (cdar call)))
              (make-llm-provider-utils-tool-use
               :name (assoc-default 'name function)
               :args (assoc-default 'arguments function))))
          (assoc-default 'tool_calls (assoc-default 'message response))))

(cl-defmethod llm-provider-populate-tool-uses ((_ llm-ollama) prompt tool-uses)
  (llm-provider-utils-append-to-prompt
   prompt
   (vconcat (mapcar (lambda (tool-use)
                      `(:function (:name ,(llm-provider-utils-tool-use-name tool-use)
                                         :arguments ,(json-serialize
                                                      (llm-provider-utils-tool-use-args tool-use)))))
                    tool-uses))))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-ollama) receiver _)
  (cons 'application/x-ndjson
        (plz-media-type:application/x-ndjson
         :handler (let ((in-reasoning))
                    (lambda (data)
                      (when-let ((response (assoc-default
                                            'content
                                            (assoc-default 'message data))))
                        ;; The response from ollama should just have the tag and
                        ;; nothing more.
                        (cond
                         ((string-match "<think>" response)
                          (setq in-reasoning t))
                         ((string-match "</think>" response)
                          (setq in-reasoning nil))
                         (t (funcall receiver (list (if in-reasoning
                                                        :reasoning
                                                      :text) response))))))))))

(cl-defmethod llm-name ((provider llm-ollama))
  (or (llm-ollama-chat-model provider)
      (llm-ollama-embedding-model provider)))

(cl-defmethod llm-chat-token-limit ((provider llm-ollama))
  (llm-provider-utils-model-token-limit (llm-ollama-chat-model provider)
                                        2048))

(cl-defmethod llm-capabilities ((provider llm-ollama))
  (append '(streaming json-response)
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
             (when (member 'tool-use capabilities) '(tool-use))
             (seq-intersection capabilities '(image-input))))))

(provide 'llm-ollama)

;;; llm-ollama.el ends here
