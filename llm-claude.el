;;; llm-claude.el --- llm module for integrating with Claude -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2024  Free Software Foundation, Inc.

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
;; This file implements the llm functionality defined in llm.el, for Claude's
;; API.

;;; Code:

(require 'llm)
(require 'llm-request)
(require 'llm-provider-utils)
(require 'rx)

;; Models defined at https://docs.anthropic.com/claude/docs/models-overview
(cl-defstruct llm-claude
  (key nil :read-only t)
  (chat-model "claude-3-opus-20240229" :read-only t))

(defun llm-claude-check-key (provider)
  "Check if the API key is valid, error if not."
  (unless (llm-claude-key provider)
    (error "No API key provided for Claude")))

(defun llm-claude-request (provider prompt stream)
  "Return the request (as an elisp JSON-convertable object).
PROVIDER contains the model name.
PROMPT is a `llm-chat-prompt' struct.
STREAM is a boolean indicating whether the response should be streamed."
  (let ((request `(("model" . ,(llm-claude-chat-model provider))
                   ("stream" . ,(if stream t :json-false))
                   ;; Claude requires max_tokens
                   ("max_tokens" . ,(or (llm-chat-prompt-max-tokens prompt) 4096))
                   ("messages" .
                    ,(mapcar (lambda (interaction)
                               `(("role" . ,(llm-chat-prompt-interaction-role interaction))
                                 ("content" . ,(llm-chat-prompt-interaction-content interaction))))
                             (llm-chat-prompt-interactions prompt)))))
        (system (llm-provider-utils-get-system-prompt prompt)))
    (when (> (length system) 0)
      (push `("system" . ,system) request))
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) request))
    request))

(defun llm-claude-get-response (response)
  "Return the content of the response from the returned value."
  (let ((content (aref (assoc-default 'content response) 0)))
    (if (equal (assoc-default 'type content) "text")
        (assoc-default 'text content)
      (format "Unsupported non-text response: %s" content))))

(cl-defmethod llm-chat ((provider llm-claude) prompt)
  (llm-claude-check-key provider)
  (let ((content (llm-claude-get-response
                  (llm-request-plz-sync "https://api.anthropic.com/v1/messages"
                                        :headers `(("x-api-key" . ,(llm-claude-key provider))
                                                   ("anthropic-version" . "2023-06-01"))
                                        :data (llm-claude-request provider prompt nil)))))
    (llm-provider-utils-append-to-prompt prompt content)
    content))

(cl-defmethod llm-chat-async ((provider llm-claude) prompt response-callback error-callback)
  (llm-claude-check-key provider)
  (let ((buf (current-buffer)))
    (llm-request-plz-async
     "https://api.anthropic.com/v1/messages"
     :headers `(("x-api-key" . ,(llm-claude-key provider))
                ("anthropic-version" . "2023-06-01"))
     :data (llm-claude-request provider prompt nil)
     :on-success
     (lambda (response)
       (let ((content (llm-claude-get-response response)))
         (llm-provider-utils-append-to-prompt prompt content)
         (llm-request-callback-in-buffer
          buf
          response-callback
          content)))
     :on-error
     (lambda (_ msg)
       (message "Error: %s" msg)
       (let ((error (assoc-default 'error msg)))
         (llm-request-callback-in-buffer
          buf error-callback
          'error
          (format "%s: %s" (assoc-default 'type error)
                  (assoc-default 'message error))))))))

;; see https://docs.anthropic.com/claude/reference/messages-streaming
(cl-defmethod llm-chat-streaming ((provider llm-claude) prompt partial-callback
                                  response-callback error-callback)
  (llm-claude-check-key provider)
  (let ((buf (current-buffer))
        (in-flight-message ""))
    (llm-request-plz-event-stream
     "https://api.anthropic.com/v1/messages"
     :headers `(("x-api-key" . ,(llm-claude-key provider))
                ("anthropic-version" . "2023-06-01"))
     :data (llm-claude-request provider prompt t)
     :event-stream-handlers
     ;; We ignore many types of messages; these might become important if Claude
     ;; sends a few different alternate contents, but for now they don't do
     ;; that.
     `(("message_start" . ,(lambda (_)))
       ("content_block_start" . ,(lambda (_)))
       ("ping" . ,(lambda (_)))
       ("message_stop" . ,(lambda (_)))
       ("content_block_stop" . ,(lambda (_)))
       ("content_block_delta" .
        ,(lambda (data)
           (setq in-flight-message
                 (concat in-flight-message
                         (let* ((json (json-parse-string data :object-type 'alist))
                                (delta (assoc-default 'delta json))
                                (type (assoc-default 'type delta)))
                           (when (equal type "text_delta")
                             (assoc-default 'text delta)))))
           (llm-request-callback-in-buffer
            buf
            partial-callback
            in-flight-message))))
     :on-success
     (lambda (_)
       (llm-provider-utils-append-to-prompt prompt in-flight-message)
       (llm-request-callback-in-buffer
        buf
        response-callback
        in-flight-message))
     :on-error
     (lambda (_ msg)
       (message "Error: %s" msg)
       (let ((error (assoc-default 'error msg)))
         (llm-request-callback-in-buffer
          buf error-callback
          'error
          (format "%s: %s" (assoc-default 'type error)
                  (assoc-default 'message error))))))))

;; See https://docs.anthropic.com/claude/docs/models-overview
(cl-defmethod llm-chat-token-limit ((provider llm-claude))
  (pcase (llm-claude-chat-model provider)
    ("claude-2.0" 100000)
    ("claude-instant-1.2" 100000)
    (_ 200000)))

(cl-defmethod llm-name ((_ llm-claude))
  "Claude")

(cl-defmethod llm-capabilities ((_ llm-claude))
  (list 'streaming))

(provide 'llm-claude)

;;; llm-claude.el ends here
