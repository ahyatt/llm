;;; llm-ollama.el --- llm module for integrating with Ollama. -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

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
;; interface to running LLMs locally. Ollama can be found at https://ollama.ai/.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-request-plz)
(require 'llm-provider-utils)
(require 'json)

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

(cl-defstruct llm-ollama
  "A structure for holding information needed by Ollama's API.

SCHEME is the http scheme to use, a string. It is optional and
default to `http'.

HOST is the host that Ollama is running on. It is optional and
default to localhost.

PORT is the localhost port that Ollama is running on.  It is optional.

CHAT-MODEL is the model to use for chat queries. It is required.

EMBEDDING-MODEL is the model to use for embeddings.  It is required."
  (scheme "http") (host "localhost") (port 11434) chat-model embedding-model)

;; Ollama's models may or may not be free, we have no way of knowing. There's no
;; way to tell, and no ToS to point out here.
(cl-defmethod llm-nonfree-message-info ((provider llm-ollama))
  (ignore provider)
  nil)

(defun llm-ollama--url (provider method)
  "With ollama PROVIDER, return url for METHOD."
  (format "%s://%s:%d/api/%s" (llm-ollama-scheme provider )(llm-ollama-host provider)
          (llm-ollama-port provider) method))

(defun llm-ollama--embedding-request (provider string)
  "Return the request to the server for the embedding of STRING.
PROVIDER is the llm-ollama provider."
  `(("prompt" . ,string)
    ("model" . ,(llm-ollama-embedding-model provider))))

(defun llm-ollama--embedding-extract-response (response)
  "Return the embedding from the server RESPONSE."
  (assoc-default 'embedding response))

(cl-defmethod llm-embedding-async ((provider llm-ollama) string vector-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-plz-async (llm-ollama--url provider "embeddings")
                           :data (llm-ollama--embedding-request provider string)
                           :on-success (lambda (data)
                                         (llm-request-callback-in-buffer
                                          buf vector-callback (llm-ollama--embedding-extract-response data)))
                           :on-error (lambda (_ _)
                                       ;; The problem with ollama is that it doesn't
                                       ;; seem to have an error response.
                                       (llm-request-callback-in-buffer
                                        buf error-callback 'error "Unknown error calling ollama")))))

(cl-defmethod llm-embedding ((provider llm-ollama) string)
  (llm-ollama--embedding-extract-response
   (llm-request-plz-sync (format "http://localhost:%d/api/embeddings" (or (llm-ollama-port provider) 11434))
                         :data (llm-ollama--embedding-request provider string))))

(defun llm-ollama--chat-request (provider prompt streaming)
  "From PROMPT, create the chat request data to send.
PROVIDER is the llm-ollama provider to use.
STREAMING is a boolean to control whether to stream the response."
  (let (request-alist messages options)
    (setq messages
          (mapcar (lambda (interaction)
                    `(("role" . ,(symbol-name (llm-chat-prompt-interaction-role interaction)))
                      ("content" . ,(llm-chat-prompt-interaction-content interaction))))
                  (llm-chat-prompt-interactions prompt)))
    (when (llm-chat-prompt-context prompt)
      (push `(("role" . "system")
              ("content" . ,(llm-provider-utils-get-system-prompt prompt llm-ollama-example-prelude)))
            messages))
    (push `("messages" . ,messages) request-alist)
    (push `("model" . ,(llm-ollama-chat-model provider)) request-alist)
    (push `("stream" . ,(if streaming t :json-false)) request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) options))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("num_predict" . ,(llm-chat-prompt-max-tokens prompt)) options))
    (when options (push `("options" . ,options) request-alist))
    request-alist))

(defun llm-ollama--get-response (response)
  "Return the response from the parsed json RESPONSE."
  (assoc-default 'content (assoc-default 'message response)))

(cl-defmethod llm-chat ((provider llm-ollama) prompt)
  ;; We expect to be in a new buffer with the response, which we use to store
  ;; local variables. The temp buffer won't have the response, but that's fine,
  ;; we really just need it for the local variables.
  (with-temp-buffer
    (let ((output (llm-ollama--get-response
                   (llm-request-plz-sync-raw-output 
                    (llm-ollama--url provider "chat")
                    :data (llm-ollama--chat-request provider prompt nil)
                    ;; ollama is run on a user's machine, and it can take a while.
                    :timeout llm-ollama-chat-timeout))))
      (llm-provider-utils-append-to-prompt prompt output)
      output)))

(cl-defmethod llm-chat-async ((provider llm-ollama) prompt response-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-plz-async
     (llm-ollama--url provider "chat")
     :data (llm-ollama--chat-request provider prompt nil)
     :timeout llm-ollama-chat-timeout
     :on-success (lambda (data)
                   (let ((output (llm-ollama--get-response data)))
                     (llm-provider-utils-append-to-prompt prompt data)
                     (llm-request-plz-callback-in-buffer buf response-callback output)))
     :on-error (lambda (_ data)
                 (let ((errdata (cdr (assoc 'error data))))
                   (llm-request-plz-callback-in-buffer buf error-callback 'error
                                                       (format "Problem calling Ollama: %s message: %s"
                                                               (cdr (assoc 'type errdata))
                                                               (cdr (assoc 'message errdata)))))))))

(cl-defmethod llm-chat-streaming ((provider llm-ollama) prompt partial-callback response-callback error-callback)
  (let ((buf (current-buffer))
        (response-text ""))
    (llm-request-plz-ndjson
     (llm-ollama--url provider "chat")
      :data (llm-ollama--chat-request provider prompt t)
      :on-success (lambda (response)
                    (llm-provider-utils-append-to-prompt prompt response-text)
                    (llm-request-callback-in-buffer
                     buf response-callback
                     response-text))
      :on-object (lambda (data)
                   (when-let ((response (llm-ollama--get-response data)))
                     (setq response-text (concat response-text response))
                     (llm-request-callback-in-buffer buf partial-callback response-text)))
      :on-error (lambda (_ _)
                  ;; The problem with ollama is that it doesn't
                  ;; seem to have an error response.
                  (llm-request-callback-in-buffer buf error-callback "Unknown error calling ollama")))))

(cl-defmethod llm-name ((provider llm-ollama))
  (llm-ollama-chat-model provider))

(cl-defmethod llm-chat-token-limit ((provider llm-ollama))
  (llm-provider-utils-model-token-limit (llm-ollama-chat-model provider)))

(cl-defmethod llm-capabilities ((_ llm-ollama))
  (list 'streaming 'embeddings))

(provide 'llm-ollama)

;;; llm-ollama.el ends here
