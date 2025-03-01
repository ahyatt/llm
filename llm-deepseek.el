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

(cl-defstruct (llm-deepseek (:include llm-openai-compatible
                                      (url "https://api.deepseek.com")
                                      (chat-model "deepseek-chat"))))

(cl-defmethod llm-nonfree-message-info ((_ llm-deepseek))
  "Location for the terms of service and privacy policy."
  "https://cdn.deepseek.com/policies/en-US/deepseek-terms-of-use.html")

(cl-defmethod llm-provider-extract-reasoning ((_ llm-deepseek) response)
  (when-let* ((choices (assoc-default 'choices response))
              (message (when (> (length choices) 0)
                         (assoc-default 'message (aref choices 0)))))
    (assoc-default 'reasoning_content message)))

(defun llm-deepseek--get-partial-chat-response (response)
  "Return the text and reasoning in RESPONSE.
RESPONSE can be nil if the response is complete."
  (when response
    (let* ((choices (assoc-default 'choices response))
           (delta (when (> (length choices) 0)
                    (assoc-default 'delta (aref choices 0))))
           (content (llm-provider-utils-json-val
                     (assoc-default 'content delta)))
           (reasoning (llm-provider-utils-json-val
                       (assoc-default 'reasoning_content delta))))
      (append (when content (list :text content))
              (when reasoning (list :reasoning reasoning))))))

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
  (append '(streaming)
          (when-let* ((model (llm-models-match (llm-deepseek-chat-model provider))))
            (llm-model-capabilities model))))

(provide 'llm-deepseek)

;;; llm-deepseek.el ends here
