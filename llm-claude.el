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
(require 'llm-provider-utils)
(require 'plz-event-source)
(require 'rx)

;; Models defined at https://docs.anthropic.com/claude/docs/models-overview
(cl-defstruct (llm-claude (:include llm-standard-chat-provider))
  (key nil :read-only t)
  (chat-model "claude-3-5-sonnet-20240620" :read-only t))

(cl-defmethod llm-nonfree-message-info ((_ llm-claude))
  "Return Claude's nonfree ToS."
  "https://www.anthropic.com/legal/consumer-terms")

(cl-defmethod llm-provider-prelude ((provider llm-claude))
  (unless (llm-claude-key provider)
    (error "No API key provided for Claude")))

(defun llm-claude--tool-call (call)
  "A Claude version of a function spec for CALL."
  `(("name" . ,(llm-function-call-name call))
    ("description" . ,(llm-function-call-description call))
    ("input_schema" . ,(llm-provider-utils-openai-arguments
                        (llm-function-call-args call)))))

(cl-defmethod llm-provider-chat-request ((provider llm-claude) prompt stream)
  (let ((request `(("model" . ,(llm-claude-chat-model provider))
                   ("stream" . ,(if stream t :json-false))
                   ;; Claude requires max_tokens
                   ("max_tokens" . ,(or (llm-chat-prompt-max-tokens prompt) 4096))
                   ("messages" .
                    ,(mapcar (lambda (interaction)
                               `(("role" . ,(pcase (llm-chat-prompt-interaction-role interaction)
                                              ('function 'user)
                                              ('assistant 'assistant)
                                              ('user 'user)))
                                 ("content" .
                                  ,(if (llm-chat-prompt-interaction-function-call-results interaction)
                                       (mapcar (lambda (result)
                                                 `(("type" . "tool_result")
                                                   ("tool_use_id" .
                                                    ,(llm-chat-prompt-function-call-result-call-id
                                                      result))
                                                   ("content" .
                                                    ,(llm-chat-prompt-function-call-result-result result))))
                                               (llm-chat-prompt-interaction-function-call-results interaction))
                                     (llm-chat-prompt-interaction-content interaction)))))
                             (llm-chat-prompt-interactions prompt)))))
        (system (llm-provider-utils-get-system-prompt prompt)))
    (when (llm-chat-prompt-functions prompt)
      (push `("tools" . ,(mapcar (lambda (f) (llm-claude--tool-call f))
                                 (llm-chat-prompt-functions prompt))) request))
    (when (> (length system) 0)
      (push `("system" . ,system) request))
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) request))
    (append request (llm-chat-prompt-non-standard-params prompt))))

(cl-defmethod llm-provider-extract-function-calls ((_ llm-claude) response)
  (let ((content (append (assoc-default 'content response) nil)))
    (cl-loop for item in content
             when (equal "tool_use" (assoc-default 'type item))
             collect (make-llm-provider-utils-function-call
                      :id (assoc-default 'id item)
                      :name (assoc-default 'name item)
                      :args (assoc-default 'input item)))))

(cl-defmethod llm-provider-populate-function-calls ((_ llm-claude) prompt calls)
  (llm-provider-utils-append-to-prompt
   prompt
   (mapcar (lambda (call)
             `((type . "tool_use")
               (id . ,(llm-provider-utils-function-call-id call))
               (name . ,(llm-provider-utils-function-call-name call))
               (input . ,(llm-provider-utils-function-call-args call))))
           calls)))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-claude) response)
  (let ((content (aref (assoc-default 'content response) 0)))
    (if (equal (assoc-default 'type content) "text")
        (assoc-default 'text content)
      (format "Unsupported non-text response: %s" content))))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-claude)
                                                    msg-receiver _ err-receiver)
  (cons 'text/event-stream
        (plz-event-source:text/event-stream
         :events `((message_start . ignore)
                   (content_block_start . ignore)
                   (ping . ignore)
                   (message_stop . ignore)
                   (content_block_stop . ignore)
                   (error . ,(lambda (event)
                               (funcall err-receiver (plz-event-source-event-data event))))
                   (content_block_delta
                    .
                    ,(lambda (event)
                       (let* ((data (plz-event-source-event-data event))
                              (json (json-parse-string data :object-type 'alist))
                              (delta (assoc-default 'delta json))
                              (type (assoc-default 'type delta)))
                         (when (equal type "text_delta")
                           (funcall msg-receiver (assoc-default 'text delta))))))))))

(cl-defmethod llm-provider-headers ((provider llm-claude))
  `(("x-api-key" . ,(llm-claude-key provider))
    ("anthropic-version" . "2023-06-01")
    ("anthropic-beta" . "tools-2024-04-04")))

(cl-defmethod llm-provider-chat-extract-error ((_ llm-claude) response)
  (when-let ((err (assoc-default 'error response)))
    (format "Error %s: '%s'" (assoc-default 'type err)
            (assoc-default 'message err))))

(cl-defmethod llm-provider-chat-url ((_ llm-claude))
  "Return the URL for the Claude API."
  "https://api.anthropic.com/v1/messages")

;; See https://docs.anthropic.com/claude/docs/models-overview
(cl-defmethod llm-chat-token-limit ((provider llm-claude))
  (pcase (llm-claude-chat-model provider)
    ("claude-2.0" 100000)
    ("claude-instant-1.2" 100000)
    (_ 200000)))

(cl-defmethod llm-name ((_ llm-claude))
  "Return the name of the provider."
  "Claude")

(cl-defmethod llm-capabilities ((_ llm-claude))
  (list 'streaming 'function-calls))

(cl-defmethod llm-provider-append-to-prompt ((_ llm-claude) prompt result
                                             &optional func-results)
  ;; Claude doesn't have a 'function role, so we just always use assistant here.
  ;; But if it's a function result, it considers that a 'user response, which
  ;; needs to be sent back.
  (llm-provider-utils-append-to-prompt prompt result func-results (if func-results
                                                                      'user
                                                                    'assistant)))


(provide 'llm-claude)

;;; llm-claude.el ends here
