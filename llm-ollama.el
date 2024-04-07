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
(require 'llm-request)
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

(cl-defstruct (llm-ollama (:include llm-standard-full-provider))
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

(cl-defmethod llm-provider-embedding-url ((provider llm-ollama))
  (llm-ollama--url provider "embeddings"))

(cl-defmethod llm-provider-chat-url ((provider llm-ollama))
  (llm-ollama--url provider "chat"))

(cl-defmethod llm-provider-chat-timeout ((_ llm-ollama))
  llm-ollama-chat-timeout)

(cl-defmethod llm-provider-embedding-request ((provider llm-ollama) string)
  "Return the request to the server for the embedding of STRING.
PROVIDER is the llm-ollama provider."
  `(("prompt" . ,string)
    ("model" . ,(llm-ollama-embedding-model provider))))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-ollama) response)
  "Return the embedding from the server RESPONSE."
  (assoc-default 'embedding response))

(cl-defmethod llm-provider-chat-request ((provider llm-ollama) prompt _)
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
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) options))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("num_predict" . ,(llm-chat-prompt-max-tokens prompt)) options))
    (when options (push `("options" . ,options) request-alist))
    request-alist))

(cl-defmethod llm-provider-embedding-extract-error ((_ llm-ollama) err-response)
  (assoc-default 'error err-response))

(cl-defmethod llm-provider-chat-extract-error ((provider llm-ollama) err-response)
  (llm-provider-embedding-extract-error provider err-response))

(defvar-local llm-ollama-current-response ""
  "The response so far from the server.")

(defvar-local llm-ollama-last-response 0
  "The last response number we've read.")

(cl-defmethod llm-provider-extract-partial-response ((_ llm-ollama) response)
  "Return the text in the partial chat response from RESPONSE."
  ;; To begin with, we should still be in the buffer with the actual response.
  (let ((current-response llm-ollama-current-response)
        (last-response llm-ollama-last-response))
    (with-temp-buffer
      (insert response)
      ;; Responses in ollama are always one per line.
      (let* ((end-pos (save-excursion (goto-char (point-max))
                                      (when (search-backward-regexp
                                             (rx (seq "done\":false}" line-end))
                                             nil t)
                                        (line-end-position)))))
        (when end-pos
          (let ((all-lines (seq-filter
                            (lambda (line) (string-match-p (rx (seq string-start ?{)) line))
                            (split-string (buffer-substring-no-properties 1 end-pos) "\n" t))))
            (setq
             current-response
             (concat current-response
                     (mapconcat
                      (lambda (line) (assoc-default 'content (assoc-default 'message (json-read-from-string line))))
                      ;; Take from response output last-response to the end. This
                      ;; counts only valid responses, so we need to throw out all
                      ;; other lines that aren't valid JSON.
                      (seq-subseq all-lines last-response) "")))
            (setq last-response (length all-lines))))))
    ;; If there is no new content, don't manipulate anything.
    (when (> (length current-response) (length llm-ollama-current-response))
      (setq llm-ollama-last-response last-response)
      (setq llm-ollama-current-response current-response))
    current-response))

(defun llm-ollama--get-final-response (response)
  "Return the final post-streaming json output from RESPONSE."
  (with-temp-buffer
    (insert response)
    ;; Find the last json object in the buffer.
    (goto-char (point-max))
    (search-backward "{" nil t)
    (json-read)))

;; Ollama chat is a streaming API, so we need to handle it differently tha normal.

(cl-defmethod llm-chat ((provider llm-ollama) prompt)
  ;; We expect to be in a new buffer with the response, which we use to store
  ;; local variables. The temp buffer won't have the response, but that's fine,
  ;; we really just need it for the local variables.
  (with-temp-buffer
    (let ((output (llm-request-sync-raw-output 
                   (llm-provider-chat-url provider)
                   :data (llm-provider-chat-request provider prompt t)
                   ;; ollama is run on a user's machine, and it can take a while.
                   :timeout llm-ollama-chat-timeout)))
      (setf (llm-chat-prompt-interactions prompt)
	        (append (llm-chat-prompt-interactions prompt)
                    (list (make-llm-chat-prompt-interaction
                           :role 'assistant
                           :content (assoc-default 'context (llm-ollama--get-final-response output))))))
      (llm-provider-extract-partial-response provider output))))

(cl-defmethod llm-chat-async ((provider llm-ollama) prompt response-callback error-callback)
  (llm-chat-streaming provider prompt #'ignore response-callback error-callback))

(cl-defmethod llm-name ((provider llm-ollama))
  (llm-ollama-chat-model provider))

(cl-defmethod llm-chat-token-limit ((provider llm-ollama))
  (llm-provider-utils-model-token-limit (llm-ollama-chat-model provider)))

(cl-defmethod llm-capabilities ((_ llm-ollama))
  (list 'streaming 'embeddings))

(provide 'llm-ollama)

;;; llm-ollama.el ends here
