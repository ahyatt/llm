;;; llm-flows.el --- Utilities for LLM workflows -*- lexical-binding: t -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
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
;; This file contains utility functions for defining and executing more
;; complicated LLM workflows.
;;
;; Everything in this file is subject to change, as ideas about how to do this
;; best for emacs are still being refined. Treat everything here as
;; alpha quality.

;;; Code:

(require 'llm)
(require 'ediff)

(cl-defstruct llm-flows-state
  "State of the current workflow.
NAME is the name of the state machine. PROVIDER is the LLM,
PROMPT is the prompt that was given to the user, and perhaps
added to in further conversation."
  name
  provider
  prompt)

(defun llm-flows-prompt-for-revision (state)
  "Prompt a revision for the diff.

Returns the new response.

STATE is the state of the workflow."
  (let ((prompt (read-from-minibuffer "Revision prompt: ")))
    (message "Getting response from %s" (llm-name (llm-flows-state-provider state)))
    (llm-chat-prompt-append-response (llm-flows-state-prompt state)
                                     prompt)
    (llm-chat (llm-flows-state-provider state)
              (llm-flows-state-prompt state))))

(defun llm-flows-check-result-diff (state before after on-accept)
  "Ask the user AFTER should be kept, given BEFORE state.
NEXT is the next state to execute, when the diff is accepted.
ON-ACCEPT is called with the final text."
  (unless (string= before after)
    (let ((buf-begin (get-buffer-create (format "*%s diff before*"
                                                (llm-flows-state-name state))))
          (buf-end (get-buffer-create (format "*%s diff after*"
                                              (llm-flows-state-name state))))
          (orig-ediff-quit-hook ediff-quit-hook))
      (with-current-buffer buf-begin
        (erase-buffer)
        (insert before)
        (add-hook 'ediff-quit-hook
                  (lambda ()
                    (setq ediff-quit-hook orig-ediff-quit-hook)
                    (let ((choice (let ((read-answer-short t))
                                    (read-answer "Accept this change? "
                                                 '(("accept" ?a "accept the change")
                                                   ("revise" ?r "ask llm for revision")
                                                   ("quit" ?q "exit")))))
                          (after (with-current-buffer buf-end (buffer-string))))
                      (kill-buffer buf-begin)
                      (kill-buffer buf-end)
                      (pcase choice
                        ("accept" (funcall on-accept after))
                        ("revise" (llm-flows-check-result-diff
                                   state before
                                   (llm-flows-prompt-for-revision state)
                                   on-accept))
                        ("quit" nil))))))
      (with-current-buffer buf-end
        (erase-buffer)
        (insert after))
      (ediff-buffers buf-begin buf-end))))

(defun llm-flows-replace-region (provider prompt name)
  "Replace the region with result of calling llm with PROMPT.
PROVIDER is the LLM provider to use. NAME is the user-visible
name for the replacement function."
  (let* ((buf (current-buffer))
         (start (region-beginning))
         (end (region-end))
         (before (buffer-substring-no-properties start end))
         (state (make-llm-flows-state :name name
                                      :provider provider
                                      :prompt prompt)))
    (message "Getting replacement from %s" (llm-name provider))
    (let* ((response (llm-chat (llm-flows-state-provider state)
                               (llm-flows-state-prompt state))))
      (llm-flows-check-result-diff state before response
                                   (lambda (text)
                                     (with-current-buffer buf
                                       (replace-region-contents start end (lambda () text))))))))

(defun llm-flows--remove-newlines (text)
  "Remove newlines from TEXT."
  (replace-regexp-in-string "\n" " " text))

(defun llm-flows--extract-json (json)
  "Extract JSON from string JSON."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    ;; If the string contains json in a markdown code block, extract it.
    (when (string-match "^```json\n\\(.*\\)\n```$" json)
      (setq json (match-string 1 json)))
    (condition-case nil
        (json-read-from-string json)
      (error (lwarn 'llm-flows :debug "Failed to parse output: %s" json)
             nil))))

(defun llm-flows-try (func &optional times)
  "Try FUNC TIMES times.
FUNC is a function with no arguments, will be successful if it
returns a non-nil value, and unsuccessful if it throws an error
or returns nil.

If TIMES is not specified, it defaults to 3.

This returns the first successful value, or will throw an error."
  (let ((times (or times 3))
        result)
    (while (and (> times 0)
                (not (condition-case err
                         (setq result (funcall func))
                       (error (message "Encounted error %s: %s" (car err)
                                       (cdr err))))))
      (setq times (1- times)))
    (unless (> times 0)
      (error "Failed to get a successful result"))
    result))

(provide 'llm-flows)

;;; llm-flows.el ends here
