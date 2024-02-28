;;; llm-gemini.el --- LLM implementation of Google Cloud Gemini AI -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
;; This file implements the llm functionality defined in llm.el, for Google's
;; Gemini AI. The documentation is at
;; https://ai.google.dev/tutorials/rest_quickstart.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-request)
(require 'llm-vertex)
(require 'llm-provider-utils)
(require 'json)

(cl-defstruct llm-gemini
  "A struct representing a Gemini client.

KEY is the API key for the client.
You can get this at https://makersuite.google.com/app/apikey."
  key (embedding-model "embedding-001") (chat-model "gemini-pro"))

(defun llm-gemini--embedding-url (provider)
  "Return the URL for the EMBEDDING request for STRING from PROVIDER."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:embedContent?key=%s"
          (llm-gemini-embedding-model provider)
          (llm-gemini-key provider)))

(defun llm-gemini--embedding-request (provider string)
  "Return the embedding request for STRING, using PROVIDER."
  `((model . ,(llm-gemini-embedding-model provider))
    (content . ((parts . (((text . ,string))))))))

(defun llm-gemini--embedding-response-handler (response)
  "Handle the embedding RESPONSE from Gemini."
  (assoc-default 'values (assoc-default 'embedding response)))

(cl-defmethod llm-embedding ((provider llm-gemini) string)
  (llm-vertex--handle-response
   (llm-request-sync (llm-gemini--embedding-url provider)
                     :data (llm-gemini--embedding-request provider string))
   #'llm-gemini--embedding-response-handler))

(cl-defmethod llm-embedding-async ((provider llm-gemini) string vector-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-gemini--embedding-url provider)
                       :data (llm-gemini--embedding-request provider string)
                       :on-success (lambda (data)
                                     (llm-request-callback-in-buffer
                                      buf vector-callback (llm-gemini--embedding-response-handler data)))
                       :on-error (lambda (_ data)
                                   (llm-request-callback-in-buffer
                                    buf error-callback
                                    'error (llm-vertex--error-message data))))))

;; from https://ai.google.dev/tutorials/rest_quickstart
(defun llm-gemini--chat-url (provider streaming-p)
  "Return the URL for the chat request, using PROVIDER.
If STREAMING-P is non-nil, use the streaming endpoint."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:%s?key=%s"
          (llm-gemini-chat-model provider)
          (if streaming-p "streamGenerateContent" "generateContent")
          (llm-gemini-key provider)))

(cl-defmethod llm-provider-utils-populate-function-calls ((_ llm-gemini) prompt calls)
  (llm-provider-utils-append-to-prompt
   prompt
   ;; For Vertex there is just going to be one call
   (mapcar (lambda (fc)
             `((functionCall
                .
                ((name . ,(llm-provider-utils-function-call-name fc))
                 (args . ,(llm-provider-utils-function-call-args fc))))))
           calls)))

(defun llm-gemini--chat-request (prompt)
  "Return the chat request for PROMPT."
  (mapcar (lambda (c) (if (eq (car c) 'generation_config)
                          (cons 'generationConfig (cdr c))
                        c))
          (llm-vertex--chat-request prompt)))

(cl-defmethod llm-chat ((provider llm-gemini) prompt)
  (llm-vertex--process-and-return
   provider prompt
   (llm-request-sync (llm-gemini--chat-url provider nil)
                     :data (llm-gemini--chat-request prompt))))

(cl-defmethod llm-chat-async ((provider llm-gemini) prompt response-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-gemini--chat-url provider nil)
                       :data (llm-gemini--chat-request prompt)
                       :on-success (lambda (data)
                                     (llm-request-callback-in-buffer
                                      buf response-callback
                                      (llm-vertex--process-and-return
                                       provider prompt
                                       data)))
                       :on-error (lambda (_ data)
                                   (llm-request-callback-in-buffer buf error-callback 'error
                                                                   (llm-vertex--error-message data))))))

(cl-defmethod llm-chat-streaming ((provider llm-gemini) prompt partial-callback response-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-gemini--chat-url provider t)
                       :data (llm-gemini--chat-request prompt)
                       :on-partial (lambda (partial)
                                     (when-let ((response (llm-vertex--get-partial-chat-response partial)))
                                       (when (> (length response) 0)
                                         (llm-request-callback-in-buffer buf partial-callback response))))
                       :on-success (lambda (data)
                                     (llm-request-callback-in-buffer
                                        buf response-callback
                                        (llm-vertex--process-and-return
                                         provider prompt data)))
                       :on-error (lambda (_ data)
                                 (llm-request-callback-in-buffer buf error-callback 'error
                                                                 (llm-vertex--error-message data))))))

(defun llm-gemini--count-token-url (provider)
  "Return the URL for the count token call, using PROVIDER."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:countTokens?key=%s"
          (llm-gemini-chat-model provider)
          (llm-gemini-key provider)))

(cl-defmethod llm-count-tokens ((provider llm-gemini) string)
  (llm-vertex--handle-response
   (llm-request-sync (llm-gemini--count-token-url provider)
                     :data (llm-vertex--to-count-token-request
                            (llm-vertex--chat-request (llm-make-simple-chat-prompt string))))
   #'llm-vertex--count-tokens-extract-response))

(cl-defmethod llm-name ((_ llm-gemini))
  "Return the name of PROVIDER."
  "Gemini")

;; From https://ai.google.dev/models/gemini.
(cl-defmethod llm-chat-token-limit ((provider llm-gemini))
  (llm-vertex--chat-token-limit (llm-gemini-chat-model provider)))

(cl-defmethod llm-capabilities ((_ llm-gemini))
  (list 'streaming 'embeddings 'function-calls))

(provide 'llm-gemini)

;;; llm-gemini.el ends here
