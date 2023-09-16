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
(require 'request)
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

(defun llm-openai--embedding-make-request (provider string vector-callback error-callback sync)
  "Make a request to Open AI to get an embedding for STRING.
PROVIDER, VECTOR-CALLBACK and ERROR-CALLBACK are as in the
`llm-embedding-async' call. SYNC is non-nil when the request
should wait until the response is received."
  (unless (llm-openai-key provider)
    (error "To call Open AI API, add a key to the `llm-openai' provider."))
  (request "https://api.openai.com/v1/embeddings"
    :type "POST"
    :sync sync
    :timeout 5
    :headers `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider)))
               ("Content-Type" . "application/json"))
    :data (json-encode `(("input" . ,string) ("model" . ,(or (llm-openai-embedding-model provider) "text-embedding-ada-002"))))
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys)
                            (funcall vector-callback
                                     (cdr (assoc 'embedding (aref (cdr (assoc 'data data)) 0))))))
    :error (cl-function (lambda (&key error-thrown data &allow-other-keys)
                          (funcall error-callback 'error
                                   (format "Problem calling Open AI: %s, type: %s message: %s"
                                           (cdr error-thrown)
                                           (assoc-default 'type (cdar data))
                                           (assoc-default 'message (cdar data))))))))

(cl-defmethod llm-embedding-async ((provider llm-openai) string vector-callback error-callback)
  (llm-openai--embedding-make-request provider string vector-callback error-callback nil))

(cl-defmethod llm-embedding ((provider llm-openai) string)
  (let ((response))
    (llm-openai--embedding-make-request provider string
                                        (lambda (vector) (setq response vector))
                                        (lambda (_ error-message) (error error-message)) t)
    response))

(defun llm-openai--chat (provider prompt response-callback error-callback &optional return-json-spec sync)
  "Main method to send a PROMPT as a chat prompt to Open AI.
RETURN-JSON-SPEC, if specified, is a JSON spec to return from the
Open AI API.

PROVIDER is a `llm-openai' struct which holds the key and other options.

RESPONSE-CALLBACK is a function to call with the LLM response.

ERROR-CALLBACK is called if there is an error, with the error
signal and message.

SYNC is non-nil when the request should wait until the response is received."
  (unless (llm-openai-key provider)
    (error "To call Open AI API, the key must have been set"))
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

    (request "https://api.openai.com/v1/chat/completions"
      :type "POST"
      :sync sync
      :headers `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider)))
                 ("Content-Type" . "application/json"))
      :data (json-encode request-alist)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((result (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices data)) 0))))))
                        (func-result (cdr (assoc 'arguments (cdr (assoc 'function_call (cdr (assoc 'message (aref (cdr (assoc 'choices data)) 0)))))))))
                    (funcall response-callback (or func-result result)))))
      :error (cl-function (lambda (&key error-thrown data &allow-other-keys)
                            (funcall error-callback
                                     'error
                                     (format "Problem calling Open AI: %s, type: %s message: %s"
                                             (cdr error-thrown)
                                             (assoc-default 'type (cdar data))
                                             (assoc-default 'message (cdar data)))))))))

(cl-defmethod llm-chat-async ((provider llm-openai) prompt response-callback error-callback)
  (llm-openai--chat provider prompt response-callback error-callback))

(cl-defmethod llm-chat ((provider llm-openai) prompt)
  (let ((response))
    (llm-openai--chat provider prompt
                               (lambda (result) (setq response result))
                               (lambda (_ msg) (error msg))
                               nil t)
    response))

(provide 'llm-openai)

;;; llm-openai.el ends here
