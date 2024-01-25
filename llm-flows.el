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

(cl-defstruct llm-flows-acceptor
  "A function that tests the output from an LLM.

SYMBOL is the symbol that will be used to refer to this acceptor.

FUNCTION is the function that will be called with a
`llm-flows-loop' arg.

REPEAT-LIMIT is the number of times this acceptor can be called.
If nil, there is no limit.
"

  symbol
  function
  repeat-limit)

(cl-defstruct llm-flows-loop
  "State and setup of a single loop in an LLM workflow.

A loop is a single operation, one that can be has acceptance
criteria and may be repeated in a loop until the acceptance tests
pass.

NAME is the name of the state machine. PROVIDER is the LLM,
PROMPT is the prompt that was given to the user, and perhaps
added to in further conversation.

acceptor is a list of `llm-flows-acceptor' structs.

LAST-RESPONSE is the last accepted response from the LLM.

TRACE is a list of the last acceptor symbols, from latest to
earliest. It keeps getting appended to whenever the LLM runs in
a loop, so it may contain acceptors that have yet to be run in
the current loop. `nil' values indicate the loop was restarted.

FINALIZER is a function that can be called with the loop to
affect the change."
  name
  provider
  prompt
  acceptors
  response
  finalizer
  trace)

(defun llm-flows-advance-loop (loop)
  "After an acceptor has succeeded, advance the LOOP state.
This just calls the next acceptor, if there is one, or if there
is no more, the finalizer."
  ;; Get the next acceptor, or nil if there are no more.
  (let* ((a (llm-flows-loop-acceptors loop))
         (last-acceptor-sym (car (llm-flows-loop-trace loop))))
    (when last-acceptor-sym
      (while (and a last-acceptor-sym
                (not (eq (llm-flows-acceptor-symbol (car a)) last-acceptor-sym)))
        (setq a (cdr a)))
      ;; We are now at the current acceptor, set the list so it's the remaining
      ;; acceptors.
      (setq a (cdr a)))    
    (if a
        (let ((next-acceptor (car a)))
          (push (llm-flows-acceptor-symbol next-acceptor) (llm-flows-loop-trace loop))
          (if (funcall (llm-flows-acceptor-function next-acceptor) loop)
              (llm-flows-advance-loop loop)
            (llm-flows-restart-loop loop)))
      (funcall (llm-flows-loop-finalizer loop) loop))))

(defun llm-flows-restart-loop (loop)
  "Restart LOOP if possible, due to an acceptor failure."
  (let* ((failed-acceptor
          (seq-find
           (lambda (a) (eq (llm-flows-acceptor-symbol a)
                           (car (llm-flows-loop-trace loop))))
           (llm-flows-loop-acceptors loop)))
         (prev-count
          (seq-count (lambda (sym)
                       (eq sym (llm-flows-acceptor-symbol failed-acceptor)))
                     (llm-flows-loop-trace loop))))
    (if (and (llm-flows-acceptor-repeat-limit failed-acceptor)
             (>= prev-count (llm-flows-acceptor-repeat-limit failed-acceptor)))
        (error "LLM flow loop %s unable to complete. Acceptance limit reached for %s"
               (llm-flows-loop-name loop)
               (llm-flows-acceptor-symbol failed-acceptor))
      (push nil (llm-flows-loop-trace loop))
      (setf (llm-flows-loop-response loop)
            (llm-chat (llm-flows-loop-provider loop)
                      (llm-flows-loop-prompt loop)))
      (llm-flows-advance-loop loop))))

(defun llm-flows-prompt-for-revision (loop)
  "Prompt a revision for the diff.

Returns the new response."
  (llm-chat-prompt-append-response (llm-flows-loop-prompt loop)
                                   (read-from-minibuffer "Revision prompt: ")))

(defun llm-flows-check-result-diff (loop)
  "Ask the user to accept the result of the LLM."
  (unless (string= before after)
    (let ((buf-begin (get-buffer-create (format "*%s diff before*"
                                                (llm-flows-loop-name loop))))
          (buf-end (get-buffer-create (format "*%s diff after*"
                                              (llm-flows-loop-name loop))))
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

(defun llm-flows-acceptor-verify (flow)
  "Acceptor that has the user verify the results of FLOW."
  )

(defun llm-flows-user-refinement (provider prompt name after)
  "Replace the region with result of calling llm with PROMPT.
PROVIDER is the LLM provider to use.

NAME is the user-visible name for the replacement function.

AFTER is the function to use after the user has accepted the
change. It is called with the accepted response from the LLM."
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
      (llm-flows-check-result-diff state before response after))))

(defun llm-flows--fill-template (text var-alist)
  "Fill variables in TEXT with VAR-ALIST.
TEXT can have variables in it marked with a double curly
brackets, so {{var}}, for example, will be replaced with the cdr
of the cons in VAR-ALIST where car is matches var. You can escape
this by adding a backlash before the curly brackets, which will
cause the curly brackets to not be detected as a variable. Note
that when writing strings, backslashes have to be escaped
themselves, so you need to write \\ to escape the curly brackets.

VAR can only be alphanumeric letters and dashes.

If TEXT has variables that cannot be found in VAR-ALIST, an error
is thrown.

The return value is the filled in text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward
            (rx (seq (group-n 1 (not ?\\))
                     "{{"
                     (group-n 2 (1+ (or alpha ?-)))
                     "}}")) nil t)
      (let ((var (match-string 2)))
        (let ((value (assoc-default var var-alist)))
          (unless value
            (error "Variable %s not found" var))
          (replace-match (concat (match-string 1) value)))))
    (buffer-string)))

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
