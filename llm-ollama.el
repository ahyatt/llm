;;; llm-ollama.el --- llm module for integrating with Ollama. -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023, 2024  Free Software Foundation, Inc.

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

(cl-defmethod llm-provider-embedding-url ((provider llm-ollama))
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
  `(("input" . ,string)
    ("model" . ,(llm-ollama-embedding-model provider))))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-ollama) response)
  "Return the embedding from the server RESPONSE."
  (aref (assoc-default 'embeddings response) 0))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-ollama) response)
  "Return the chat response from the server RESPONSE."
  (assoc-default 'content (assoc-default 'message response)))

(cl-defmethod llm-provider-chat-request ((provider llm-ollama) prompt streaming)
  (let (request-alist messages options)
    (setq messages
          (mapcar (lambda (interaction)
                    `(("role" . ,(symbol-name (llm-chat-prompt-interaction-role interaction)))
                      ("content" . ,(let ((content
                                           (llm-chat-prompt-interaction-content interaction)))
                                      (if (stringp content)
                                          content
                                        (json-encode content))))))
                  (llm-chat-prompt-interactions prompt)))
    (when (llm-chat-prompt-context prompt)
      (push `(("role" . "system")
              ("content" . ,(llm-provider-utils-get-system-prompt prompt llm-ollama-example-prelude)))
            messages))
    (push `("messages" . ,messages) request-alist)
    (push `("model" . ,(llm-ollama-chat-model provider)) request-alist)
    (when (and streaming (llm-chat-prompt-functions prompt))
      (signal 'not-implemented
              "Ollama does not support streaming with function calls"))
    (when (llm-chat-prompt-functions prompt)
      (push `("tools" . ,(mapcar #'llm-provider-utils-openai-function-spec
                                 (llm-chat-prompt-functions prompt))) request-alist))
    (push `("stream" . ,(if streaming t :json-false)) request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) options))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("num_predict" . ,(llm-chat-prompt-max-tokens prompt)) options))
    (setq options (append options (llm-chat-prompt-non-standard-params prompt)))
    (when options (push `("options" . ,options) request-alist))
    request-alist))

(cl-defmethod llm-provider-extract-function-calls ((_ llm-ollama) response)
  (mapcar (lambda (call)
            (let ((function (cdar call)))
              (make-llm-provider-utils-function-call
               :name (assoc-default 'name function)
               :args (assoc-default 'arguments function))))
          (assoc-default 'tool_calls (assoc-default 'message response))))

(cl-defmethod llm-provider-populate-function-calls ((_ llm-ollama) prompt calls)
  (llm-provider-utils-append-to-prompt
   prompt
   (mapcar (lambda (call)
             `((function (name . ,(llm-provider-utils-function-call-name call))
                         (arguments . ,(json-encode
                                        (llm-provider-utils-function-call-args call))))))
           calls)))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-ollama) msg-receiver _ _)
  (cons 'application/x-ndjson
        (plz-media-type:application/x-ndjson
         :handler (lambda (data)
                    (when-let ((response (assoc-default
                                          'content
                                          (assoc-default 'message data))))
                      (funcall msg-receiver response))))))

(cl-defmethod llm-name ((provider llm-ollama))
  (llm-ollama-chat-model provider))

(cl-defmethod llm-chat-token-limit ((provider llm-ollama))
  (llm-provider-utils-model-token-limit (llm-ollama-chat-model provider)))

(cl-defmethod llm-capabilities ((provider llm-ollama))
  (append (list 'streaming)
          ;; See https://ollama.com/search?q=&c=embedding
          (when (and (llm-ollama-embedding-model provider)
                     (string-match
                      (rx (or "nomic-embed-text"
                              "mxbai-embed-large"
                              "all-minilm"
                              "snowflake-arctic-embed"))
                      (llm-ollama-embedding-model provider)))
            (list 'embeddings))
          ;; see https://ollama.com/search?c=tools
          (when (string-match
                 (rx (or "llama3.1" "mistral-nemo" "mistral-large"
                         "mistral" "mixtral" "command-r-plus"
                         "llama3-groq-tool-use"
                         "firefunction-v2"))
                 (llm-ollama-chat-model provider))
            (list 'function-calls))))

(provide 'llm-ollama)

;;; llm-ollama.el ends here
