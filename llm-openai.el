;;; llm-openai.el --- llm module for integrating with Open AI -*- lexical-binding: t -*-

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
;; This file implements the llm functionality defined in llm.el, for Open AI's
;; API.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-request)
(require 'json)

(defgroup llm-openai nil
  "LLM implementation for Open AI."
  :group 'llm)

(defcustom llm-openai-example-prelude "Examples of how you should respond follow."
  "The prelude to use for examples in Open AI chat prompts."
  :type 'string
  :group 'llm-openai)

(cl-defstruct llm-openai
  "A structure for holding information needed by Open AI's API.

KEY is the API key for Open AI, which is required.

CHAT-MODEL is the model to use for chat queries. If unset, it
will use a reasonable default.

EMBEDDING-MODEL is the model to use for embeddings.  If unset, it
will use a reasonable default."
  key chat-model embedding-model)

(cl-defmethod llm-nonfree-message-info ((provider llm-openai))
  (ignore provider)
  (cons "Open AI" "https://openai.com/policies/terms-of-use"))

(defun llm-openai--embedding-request (provider string)
  "Return the request to the server for the embedding of STRING.
PROVIDER is the llm-openai provider."
  `(("input" . ,string)
    ("model" . ,(or (llm-openai-embedding-model provider) "text-embedding-ada-002"))))

(defun llm-openai--embedding-extract-response (response)
  "Return the embedding from the server RESPONSE."
  (cdr (assoc 'embedding (aref (cdr (assoc 'data response)) 0))))

(defun llm-openai--error-message (err-response)
  "Return a user-visible error message from ERR-RESPONSE."
  (let ((errdata (cdr (assoc 'error err-response))))
    (format "Problem calling Open AI: %s message: %s"
            (cdr (assoc 'type errdata))
            (cdr (assoc 'message errdata)))))

(defun llm-openai--handle-response (response extractor)
  "If RESPONSE is an error, throw it, else call EXTRACTOR."
  (if (cdr (assoc 'error response))
      (error (llm-openai--error-message response))
    (funcall extractor response)))

(cl-defmethod llm-embedding-async ((provider llm-openai) string vector-callback error-callback)
  (unless (llm-openai-key provider)
    (error "To call Open AI API, add a key to the `llm-openai' provider."))
  (llm-request-async "https://api.openai.com/v1/embeddings"
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider))))
                     :data (llm-openai--embedding-request provider string)
                     :on-success (lambda (data)
                                   (funcall vector-callback (llm-openai--embedding-extract-response data)))
                     :on-error (lambda (_ data) 
                                 (funcall error-callback 'error
                                            (llm-openai--error-message data)))))

(cl-defmethod llm-embedding ((provider llm-openai) string)
  (unless (llm-openai-key provider)
    (error "To call Open AI API, add a key to the `llm-openai' provider."))
  (llm-openai--handle-response
   (llm-request-sync "https://api.openai.com/v1/embeddings"
               :headers `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider))))
               :data (llm-openai--embedding-request provider string))
   #'llm-openai--embedding-extract-response))

(defun llm-openai--chat-request (provider prompt &optional return-json-spec streaming)
  "From PROMPT, create the chat request data to send.
PROVIDER is the llm-openai provider to use.
RETURN-JSON-SPEC is the optional specification for the JSON to return.
STREAMING if non-nil, turn on response streaming."
  (let (request-alist system-prompt)
    (when (llm-chat-prompt-context prompt)
      (setq system-prompt (llm-chat-prompt-context prompt)))
    (when (llm-chat-prompt-examples prompt)
      (setq system-prompt
            (concat (if system-prompt (format "\n%s\n" system-prompt) "")
                    llm-openai-example-prelude
                    "\n"
                    (mapconcat (lambda (example)
                                 (format "User: %s\nAssistant: %s"
                                         (car example)
                                         (cdr example)))
                               (llm-chat-prompt-examples prompt) "\n"))))
    (when system-prompt
      (push (make-llm-chat-prompt-interaction :role 'system :content system-prompt)
            (llm-chat-prompt-interactions prompt)))
    (when streaming (push `("stream" . ,t) request-alist))
    (push `("messages" . ,(mapcar (lambda (p)
                                    `(("role" . ,(pcase (llm-chat-prompt-interaction-role p)
                                                   ('user "user")
                                                   ('system "system")
                                                   ('assistant "assistant")))
                                      ("content" . ,(string-trim (llm-chat-prompt-interaction-content p)))))
                                  (llm-chat-prompt-interactions prompt)))
          request-alist)
    (push `("model" . ,(or (llm-openai-chat-model provider) "gpt-3.5-turbo-0613")) request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(/ (llm-chat-prompt-temperature prompt) 2.0)) request-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("max_tokens" . ,(llm-chat-prompt-max-tokens prompt)) request-alist))
    (when return-json-spec
      (push `("functions" . ((("name" . "output")
                              ("parameters" . ,return-json-spec))))
            request-alist)
      (push '("function_call" . (("name" . "output"))) request-alist))
    request-alist))

(defun llm-openai--extract-chat-response (response)
  "Return chat response from server RESPONSE."
  (let ((result (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0))))))
        (func-result (cdr (assoc 'arguments (cdr (assoc 'function_call (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0)))))))))
    (or func-result result)))

(cl-defmethod llm-chat-async ((provider llm-openai) prompt response-callback error-callback)
  (unless (llm-openai-key provider)
    (error "To call Open AI API, the key must have been set"))
  (llm-request-async "https://api.openai.com/v1/chat/completions"
      :headers `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider))))
      :data (llm-openai--chat-request provider prompt)
      :on-success (lambda (data) (funcall response-callback (llm-openai--extract-chat-response data)))
      :on-error (lambda (_ data)
                  (let ((errdata (cdr (assoc 'error data))))
                    (funcall error-callback 'error
                             (format "Problem calling Open AI: %s message: %s"
                                     (cdr (assoc 'type errdata))
                                     (cdr (assoc 'message errdata))))))))

(cl-defmethod llm-chat ((provider llm-openai) prompt)
  (unless (llm-openai-key provider)
    (error "To call Open AI API, the key must have been set"))
  (llm-openai--handle-response
   (llm-request-sync "https://api.openai.com/v1/chat/completions"
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider))))
                     :data (llm-openai--chat-request provider prompt))
   #'llm-openai--extract-chat-response))

(defvar-local llm-openai-current-response ""
  "The response so far from the server.")

(defvar-local llm-openai-last-position 1
  "The last position in the streamed response we read until.")

(defun llm-openai--get-partial-chat-response (response)
  "Return the text in the partial chat response from RESPONSE."
  ;; To begin with, we should still be in the buffer with the actual response.
  (let ((current-response llm-openai-current-response)
        (last-position llm-openai-last-position))
    (with-temp-buffer
      (insert response)
      (goto-char last-position)
      (when (search-forward "\ndata: {" nil t)
        (backward-char 2)
        (ignore-errors
          (setq current-response
                (concat current-response (assoc-default 'content (assoc-default 'delta (aref (assoc-default 'choices (json-read)) 0))))))
        (setq last-position (point))))
    (setq-local llm-openai-current-response current-response)
    (setq-local llm-openai-last-position last-position)
    current-response))

(cl-defmethod llm-chat-streaming ((provider llm-openai) prompt partial-callback response-callback error-callback)
  (unless (llm-openai-key provider)
    (error "To call Open AI API, the key must have been set"))
  (llm-request-async "https://api.openai.com/v1/chat/completions"
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider))))
                     :data (llm-openai--chat-request provider prompt nil t)
                     :on-error (lambda (_ data)
                                 (let ((errdata (cdr (assoc 'error data))))
                                   (funcall error-callback 'error
                                            (format "Problem calling Open AI: %s message: %s"
                                                    (cdr (assoc 'type errdata))
                                                    (cdr (assoc 'message errdata))))))
                     :on-partial (lambda (data)
                                   (when-let ((response (llm-openai--get-partial-chat-response data)))
                                     (funcall partial-callback response)))
                     :on-success-raw (lambda (data)
                                       (funcall response-callback (llm-openai--get-partial-chat-response data)))))

(provide 'llm-openai)

;;; llm-openai.el ends here
