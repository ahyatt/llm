;;; llm-ollama.el --- llm module for integrating with Ollama. -*- lexical-binding: t -*-

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
    (llm-request-async (llm-ollama--url provider "embeddings")
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
   (llm-request-sync (format "http://localhost:%d/api/embeddings" (or (llm-ollama-port provider) 11434))
                     :data (llm-ollama--embedding-request provider string))))

(defun llm-ollama--chat-request (provider prompt)
  "From PROMPT, create the chat request data to send.
PROVIDER is the llm-ollama provider to use."
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

(defvar-local llm-ollama-current-response ""
  "The response so far from the server.")

(defvar-local llm-ollama-last-response 0
  "The last response number we've read.")

(defun llm-ollama--get-partial-chat-response (response)
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

(cl-defmethod llm-chat ((provider llm-ollama) prompt)
  ;; We expect to be in a new buffer with the response, which we use to store
  ;; local variables. The temp buffer won't have the response, but that's fine,
  ;; we really just need it for the local variables.
  (with-temp-buffer
    (let ((output (llm-request-sync-raw-output 
                   (llm-ollama--url provider "chat")
                   :data (llm-ollama--chat-request provider prompt)
                   ;; ollama is run on a user's machine, and it can take a while.
                   :timeout llm-ollama-chat-timeout)))
      (setf (llm-chat-prompt-interactions prompt)
	        (list (assoc-default 'context (llm-ollama--get-final-response output))))
      (llm-ollama--get-partial-chat-response output))))

(cl-defmethod llm-chat-async ((provider llm-ollama) prompt response-callback error-callback)
  (llm-chat-streaming provider prompt (lambda (_)) response-callback error-callback))

(cl-defmethod llm-chat-streaming ((provider llm-ollama) prompt partial-callback response-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-ollama--url provider "chat")
      :data (llm-ollama--chat-request provider prompt)
      :on-success-raw (lambda (response)
                        (setf (llm-chat-prompt-interactions prompt)
                              (list (assoc-default 'context (llm-ollama--get-final-response response))))
                        (llm-request-callback-in-buffer
                         buf response-callback
                         (llm-ollama--get-partial-chat-response response)))
      :on-partial (lambda (data)
                    (when-let ((response (llm-ollama--get-partial-chat-response data)))
                      (llm-request-callback-in-buffer buf partial-callback response)))
      :on-error (lambda (_ _)
                  ;; The problem with ollama is that it doesn't
                  ;; seem to have an error response.
                  (llm-request-callback-in-buffer buf error-callback "Unknown error calling ollama")))))

(cl-defmethod llm-name ((provider llm-ollama))
  (llm-ollama-chat-model provider))

(cl-defmethod llm-chat-token-limit ((provider llm-ollama))
  (llm-provider-utils-model-token-limit (llm-ollama-chat-model provider)))

(provide 'llm-ollama)

;;; llm-ollama.el ends here
