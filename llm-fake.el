;;; llm-fake.el --- Use for developers looking at llm calls. -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023, 2024  Free Software Foundation, Inc.

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
;; This file implements the llm functionality defined in llm.el, for developers
;; who want to just understand what llm calls are made, and with what data.  Or,
;; to test out various functionality they have.  The functions return something,
;; or throw errors, depending on how the `llm-fake' provider is configured.

(require 'cl-lib)
(require 'llm)

;;; Code:

(cl-defstruct llm-fake
 "A provider for the fake LLM provider.

OUTPUT-TO-BUFFER can be nil, in which case, nothing will be
output.  If a string or a buffer, it will append the request as
text to that buffer.

CHAT-ACTION-FUNC will be called with no arguments to produce
either a string response for the chat, or a signal symbol and
message cons.  If nil, the response will be a short text string.

EMBEDDING-ACTION-FUNC will be called with no arguments to produce
either a vector response for the chat, or a signal symbol and
message cons.  If nil, the response will be a simple vector."
 output-to-buffer chat-action-func embedding-action-func)

(cl-defmethod llm-chat-async ((provider llm-fake) prompt response-callback error-callback)
  (condition-case err
      (funcall response-callback (llm-chat provider prompt))
    (t (funcall error-callback (car err) (cdr err))))
  nil)

(cl-defmethod llm-chat ((provider llm-fake) prompt)
  (when (llm-fake-output-to-buffer provider)
    (with-current-buffer (get-buffer-create (llm-fake-output-to-buffer provider))
      (goto-char (point-max))
      (insert "\nCall to llm-chat\n"  (llm-chat-prompt-to-text prompt) "\n")))
  (let ((result
         (if (llm-fake-chat-action-func provider)
             (let* ((f (llm-fake-chat-action-func provider))
                    (result (funcall f)))
               (pcase (type-of result)
                 ('string result)
                 ('cons (signal (car result) (cdr result)))
                 (_ (error "Incorrect type found in `chat-action-func': %s" (type-of result)))))
           "Sample response from `llm-chat-async'")))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction :role 'assistant :content result))))
    result))

(cl-defmethod llm-chat-streaming ((provider llm-fake) prompt partial-callback response-callback _error-callback)
  (when (llm-fake-output-to-buffer provider)
    (with-current-buffer (get-buffer-create (llm-fake-output-to-buffer provider))
      (goto-char (point-max))
      (insert "\nCall to llm-chat-streaming\n"  (llm-chat-prompt-to-text prompt) "\n")))
  (let ((text "Sample response from `llm-chat-streaming'"))
    (when (llm-fake-chat-action-func provider)
      (let* ((f (llm-fake-chat-action-func provider))
             (result (funcall f)))
        (pcase (type-of result)
          ('string (setq text result))
          ('cons (signal (car result) (cdr result)))
          (_ (error "Incorrect type found in `chat-action-func': %s" (type-of result))))))
    (let ((accum ""))
      (mapc (lambda (word)
              (setq accum (concat accum word " "))
              (funcall partial-callback accum)
              (sleep-for 0 100))
            (split-string text))
      (setf (llm-chat-prompt-interactions prompt)
            (append (llm-chat-prompt-interactions prompt)
                    (list (make-llm-chat-prompt-interaction :role 'assistant :content text))))
      (funcall response-callback text))))

(cl-defmethod llm-embedding ((provider llm-fake) string)
  (when (llm-fake-output-to-buffer provider)
    (with-current-buffer (get-buffer-create (llm-fake-output-to-buffer provider))
      (goto-char (point-max))
      (insert "\nCall to llm-embedding with text: " string "\n")))
  (if (llm-fake-embedding-action-func provider)
      (let* ((f (llm-fake-embedding-action-func provider))
             (result (funcall f)))
        (pcase (type-of result)
                ('vector result)
                ('cons (signal (car result) (cdr result)))
                (_ (error "Incorrect type found in `chat-embedding-func': %s" (type-of result)))))
    [0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9]))

(cl-defmethod llm-embedding-async ((provider llm-fake) string vector-callback error-callback)
  (condition-case err
      (funcall vector-callback (llm-embedding provider string))
    (t (funcall error-callback (car err) (cdr err))))
  nil)

(cl-defmethod llm-name ((_ llm-fake))
  "The name of the provider."
  "Fake")

(cl-defmethod llm-capabilities ((_ llm-fake))
  (list 'streaming 'embeddings))

(provide 'llm-fake)
;;; llm-fake.el ends here
