;;; llm-deepseek.el --- llm module for integrating with DeepSeek's service -*- lexical-binding: t; package-lint-main-file: "llm.el"; byte-compile-docstring-max-column: 200-*-

;; Copyright (c) 2025  Free Software Foundation, Inc.

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
;; This module provides integration with DeepSeek's service.

;;; Code:

(require 'llm)
(require 'llm-openai)
(require 'llm-models)
(require 'cl-lib)

(cl-defstruct (llm-deepseek
               (:include llm-openai-compatible
                         (url "https://api.deepseek.com")
                         (chat-model "deepseek-chat"))
               (:constructor make-llm-deepseek
                             (&key default-chat-temperature
                                   default-chat-max-tokens
                                   default-chat-non-standard-params
                                   ((:key raw-key))
                                   (chat-model "deepseek-chat")
                                   (embedding-model "unset")
                                   (url "https://api.deepseek.com")
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key))))))

(cl-defmethod llm-nonfree-message-info ((_ llm-deepseek))
  "Location for the terms of service and privacy policy."
  "https://cdn.deepseek.com/policies/en-US/deepseek-terms-of-use.html")

(cl-defmethod llm-provider-extract-reasoning ((_ llm-deepseek) response)
  (when-let* ((choices (assoc-default 'choices response))
              (message (when (> (length choices) 0)
                         (assoc-default 'message (aref choices 0)))))
    (assoc-default 'reasoning_content message)))

(cl-defmethod llm-provider-extract-for-multi-turn ((provider llm-deepseek) response)
  (when-let* ((reasoning-content (llm-provider-extract-reasoning provider response)))
    (list :deepseek-reasoning-content reasoning-content)))

(cl-defmethod llm-provider-annotate-tool-uses ((_ llm-deepseek) interaction multi-turn)
  "Keep DeepSeek reasoning with assistant tool-call INTERACTION."
  (setf (llm-chat-prompt-interaction-multi-turn-plist interaction) multi-turn))

(cl-defmethod llm-provider-annotate-chat-message
  ((_ llm-deepseek) interaction message)
  "Add DeepSeek replayable reasoning from INTERACTION to MESSAGE."
  (when-let* ((reasoning-content
               (plist-get (llm-chat-prompt-interaction-multi-turn-plist interaction)
                          :deepseek-reasoning-content)))
    (setq message (plist-put message :reasoning_content reasoning-content)))
  message)

(cl-defmethod llm-provider-chat-request ((_provider llm-deepseek) _prompt _streaming)
  "Build a DeepSeek request with complete assistant tool-call messages.
Assistant text and tool calls as adjacent interactions but DeepSeek
requires one assistant message containing both."
  (let* ((request (cl-call-next-method))
         (original-messages (plist-get request :messages))
         (remaining (append original-messages nil))
         combined)
    (while remaining
      (let ((message (pop remaining))
            (next (car remaining)))
        (when (and next
                   (equal (plist-get message :role) "assistant")
                   (or (stringp (plist-get message :content))
                       (plist-get message :reasoning_content))
                   (equal (plist-get next :role) "assistant")
                   (plist-get next :tool_calls))
          (let ((content (or (plist-get message :content) :null))
                (reasoning-content
                 (or (plist-get message :reasoning_content)
                     (plist-get next :reasoning_content))))
            (setq message (pop remaining))
            (setq message (plist-put message :content content))
            (when reasoning-content
              (setq message
                    (plist-put message :reasoning_content reasoning-content)))))
        ;; Assistant content is required, even if it's null.
        (when (and (equal (plist-get message :role) "assistant")
                   (plist-get message :tool_calls)
                   (not (plist-member message :content)))
          (setq message (plist-put message :content :null)))
        (push message combined)))
    (plist-put
     request :messages
     (vconcat (nreverse combined)))))

(defun llm-deepseek--get-partial-chat-response (response)
  "Return the text and reasoning in RESPONSE.
RESPONSE can be nil if the response is complete."
  (when response
    (let* ((choices (assoc-default 'choices response))
           (usage (assoc-default 'usage response))
           (delta (when (> (length choices) 0)
                    (assoc-default 'delta (aref choices 0))))
           (tool-calls (llm-provider-utils-json-val
                        (assoc-default 'tool_calls delta)))
           (content (llm-provider-utils-json-val
                     (assoc-default 'content delta)))
           (reasoning (llm-provider-utils-json-val
                       (assoc-default 'reasoning_content delta))))
      (append (when content (list :text content))
              (when tool-calls `(:tool-uses-raw ,tool-calls))
              (when reasoning
                (list :reasoning reasoning
                      :multi-turn
                      (list :deepseek-reasoning-content reasoning)))
              (when (and usage (not (eq usage :null)))
                (list :input-tokens (assoc-default 'prompt_tokens usage)
                      :output-tokens (assoc-default 'completion_tokens usage)))))))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-deepseek) receiver _)
  (cons 'text/event-stream
        (plz-event-source:text/event-stream
         :events `((message
                    .
                    ,(lambda (event)
                       (let ((data (plz-event-source-event-data event)))
                         (unless (equal data "[DONE]")
                           (when-let* ((response (llm-deepseek--get-partial-chat-response
                                                  (json-parse-string data :object-type 'alist))))
                             (funcall receiver response))))))))))

(cl-defmethod llm-capabilities ((provider llm-deepseek))
  (append '(streaming model-list)
          (when-let* ((model (llm-models-match (llm-deepseek-chat-model provider))))
            (llm-model-capabilities model))))

(cl-defmethod llm-openai--build-reasoning ((_ llm-deepseek) prompt)
  (when (llm-chat-prompt-reasoning prompt)
    (pcase (llm-chat-prompt-reasoning prompt)
      ('none '(:thinking (:type "disabled")))
      ('light '(:thinking (:type "enabled") :reasoning_effort "low"))
      ('medium '(:thinking (:type "enabled") :reasoning_effort "medium"))
      ('maximum '(:thinking (:type "enabled") :reasoning_effort "high"))
      (_ (signal 'llm-not-supported
                 (list (format "Unknown reasoning effort option: %s"
                               (llm-chat-prompt-reasoning prompt))))))))

(provide 'llm-deepseek)

;;; llm-deepseek.el ends here
