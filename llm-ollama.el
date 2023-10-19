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

HOST is the host that Ollama is running on. It is optional and
default to localhost.

PORT is the localhost port that Ollama is running on.  It is optional.

CHAT-MODEL is the model to use for chat queries. It is required.

EMBEDDING-MODEL is the model to use for embeddings.  It is required."
  host port chat-model embedding-model)

;; Ollama's models may or may not be free, we have no way of knowing. There's no
;; way to tell, and no ToS to point out here.
(cl-defmethod llm-nonfree-message-info ((provider llm-ollama))
  (ignore provider)
  nil)

(defun llm-ollama--url (provider method)
  "With ollama PROVIDER, return url for METHOD."
  (format "http://%s:%d/api/%s" (or (llm-ollama-host provider) "localhost")
          (or (llm-ollama-port provider) 11434) method))

(defun llm-ollama--embedding-request (provider string)
  "Return the request to the server for the embedding of STRING.
PROVIDER is the llm-ollama provider."
  `(("prompt" . ,string)
    ("model" . ,(llm-ollama-embedding-model provider))))

(defun llm-ollama--embedding-extract-response (response)
  "Return the embedding from the server RESPONSE."
  (assoc-default 'embedding response))

(cl-defmethod llm-embedding-async ((provider llm-ollama) string vector-callback error-callback)
  (llm-request-async (llm-ollama--url provider "embeddings")
                     :data (llm-ollama--embedding-request provider string)
                     :on-success (lambda (data)
                                   (funcall vector-callback (llm-ollama--embedding-extract-response data)))
                     :on-error (lambda (_ _)
                                 ;; The problem with ollama is that it doesn't
                                 ;; seem to have an error response.
                                 (funcall error-callback 'error "Unknown error calling ollama"))))

(cl-defmethod llm-embedding ((provider llm-ollama) string)
  (llm-ollama--embedding-extract-response
   (llm-request-sync (format "http://localhost:%d/api/embeddings" (or (llm-ollama-port provider) 11434))
                     :data (llm-ollama--embedding-request provider string))))

(defun llm-ollama--chat-request (provider prompt)
  "From PROMPT, create the chat request data to send.
PROVIDER is the llm-ollama provider to use.
RETURN-JSON-SPEC is the optional specification for the JSON to return.
STREAMING if non-nil, turn on response streaming."
  (let (request-alist text-prompt options)
    (when (llm-chat-prompt-context prompt)
      (push `("system" . ,(llm-chat-prompt-context prompt)) request-alist))
    (when (llm-chat-prompt-examples prompt)
      (setq text-prompt
            (concat llm-ollama-example-prelude
                    "\n"
                    (mapconcat (lambda (example)
                                 (format "User: %s\nAssistant: %s"
                                         (car example)
                                         (cdr example)))
                               (llm-chat-prompt-examples prompt) "\n"))))
    (setq text-prompt (concat text-prompt "\n"
                              (let ((conversationp (> (length (llm-chat-prompt-interactions prompt)) 1)))
                                (if conversationp
                                    (concat
                                     "The following interactions have already happened: "
                                     (mapcar (lambda (p)
                                               (format "%s: %s\n"
                                                       (pcase (llm-chat-prompt-interaction-role p)
                                                         ('user "User")
                                                         ('assistant "Assistant"))
                                                       (string-trim (llm-chat-prompt-interaction-content p))))
                                             (llm-chat-prompt-interactions prompt)))
                                  (string-trim
                                   (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt))))))))
    (push `("prompt" . ,(string-trim text-prompt)) request-alist)
    (push `("model" . ,(llm-ollama-chat-model provider)) request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) options))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("num_predict" . ,(llm-chat-prompt-max-tokens prompt)) options))
    (when options (push `("options" . ,options) request-alist))
    request-alist))

(defvar-local llm-ollama-current-response ""
  "The response so far from the server.")

(defvar-local llm-ollama-last-position 1
  "The last position in the streamed response we read until.")

(defun llm-ollama--get-partial-chat-response (response)
  "Return the text in the partial chat response from RESPONSE."
  ;; To begin with, we should still be in the buffer with the actual response.
  (let ((current-response llm-ollama-current-response)
        (last-position llm-ollama-last-position))
    (with-temp-buffer
      (insert response)
      (goto-char last-position)
      (while (search-forward "{" nil t)
        (backward-char 1)
        (ignore-errors
          (let ((obj (json-read)))
            (unless (eq (assoc-default 'done obj) :json-true)
              (setq current-response
                    (concat current-response (assoc-default 'response obj))))))
        (setq last-position (point))))
    (setq-local llm-ollama-current-response current-response)
    (setq-local llm-ollama-last-position last-position)
    current-response))

(cl-defmethod llm-chat ((provider llm-ollama) prompt)
  ;; We expect to be in a new buffer with the response, which we use to store
  ;; local variables. The temp buffer won't have the response, but that's fine,
  ;; we really just need it for the local variables.
  (with-temp-buffer
    (llm-ollama--get-partial-chat-response
     (llm-request-sync-raw-output (llm-ollama--url provider "generate")
                                  :data (llm-ollama--chat-request provider prompt)
                                  ;; ollama is run on a user's machine, and it can take a while.
                                  :timeout llm-ollama-chat-timeout))))

(cl-defmethod llm-chat-async ((provider llm-ollama) prompt response-callback error-callback)
  (llm-chat-streaming provider prompt (lambda (_)) response-callback error-callback))

(cl-defmethod llm-chat-streaming ((provider llm-ollama) prompt partial-callback response-callback error-callback)
  (llm-request-async (llm-ollama--url provider "generate")
      :data (llm-ollama--chat-request provider prompt)
      :on-success-raw (lambda (data)
                        (funcall response-callback (llm-ollama--get-partial-chat-response data)))
      :on-partial (lambda (data)
                    (when-let ((response (llm-ollama--get-partial-chat-response data)))
                      (funcall partial-callback response)))
      :on-error (lambda (_ _)
                  ;; The problem with ollama is that it doesn't
                  ;; seem to have an error response.
                  (funcall error-callback 'error "Unknown error calling ollama"))))

(provide 'llm-ollama)

;;; llm-ollama.el ends here
