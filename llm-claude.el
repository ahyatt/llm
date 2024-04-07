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
(require 'plz-event-source)
(require 'rx)

;; Models defined at https://docs.anthropic.com/claude/docs/models-overview
(cl-defstruct (llm-claude (:include llm-standard-chat-provider))
  (key nil :read-only t)
  (chat-model "claude-3-opus-20240229" :read-only t))

(cl-defmethod llm-nonfree-message-info ((_ llm-claude))
  "https://www.anthropic.com/legal/consumer-terms")

(cl-defmethod llm-provider-prelude ((provider llm-claude))
  "Check if the API key is valid, error if not."
  (unless (llm-claude-key provider)
    (error "No API key provided for Claude")))

(cl-defmethod llm-provider-chat-request ((provider llm-claude) prompt stream)
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
    ("anthropic-version" . "2023-06-01")))

(cl-defmethod llm-provider-extact-error ((_ llm-claude) response)
  (assoc-default 'error response))

(cl-defmethod llm-provider-chat-url ((_ llm-claude))
  "https://api.anthropic.com/v1/messages")

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
