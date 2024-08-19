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
(require 'fsm)
(require 'cl-macs)
(require 'ediff)

(defvar llm-flows-last-fsm nil
  "The last FSM that was run.")

(cl-defstruct llm-flows-state
  "State and setup of a single loop in an LLM workflow.

A loop is a single operation, one that can be has acceptance
criteria and may be repeated in a loop until the acceptance tests
pass.

NAME is the name of the state machine. PROVIDER is the LLM,
PROMPT is the prompt that was given to the user, and perhaps
added to in further conversation.

RUN-COUNT is the number of times the LLM has been run.

JSON-VERIFIER, if non-nil, should be a function that takes in
JSON and checks to make sure it looks like it is supposed to, and
returns nil if it doesn't. If it is nil, we take that to mean
there shouldn't be JSON converstion.

USER-VERIFIED, if non-nil, is a function that takes in the fsm
and the result and asks the user to verify. It is expected to
call fsm with the a cons of one of the symbols `accept',
`repeat', `revise' or `quit', and the result (which may have been
revised by the user).

ON-SUCCESS takes the final result (text, or JSON), and does
whatever needs to be done.

ON-ERROR takes the error type and error message, and handles it
in whatever way is appropriate.

ON-UNDO is called if we want to undo whatever changes happened in
ON-SUCCESS.  It does not take any arguments."
  name
  provider
  prompt
  json-verifier
  user-verifier
  on-success
  on-error
  on-undo
  (run-count 0))

(define-fsm llm-flows-simple
            :start ((start-state)
                    "Start the LLM flow"
                    (list :chat start-state 0))
            :state-data-name state
            :states
            ((:chat
              (:enter (llm-chat-async (llm-flows-state-provider state)
                                      (llm-flows-state-prompt state)
                                      (lambda (result)
                                        (fsm-send fsm (cons :success result)))
                                      (lambda (_ errmsg)
                                        (fsm-send fsm (cons :error errmsg))))
                      (list state nil))
              (:event
               (ignore callback)
               (llm-flows-handle-llm-response fsm state event)))
             (:json-verifier
              (:event
               (ignore callback)
               (llm-flows-json-extract-and-verify fsm state event)))
             (:user-verifier
              (:event
               (ignore callback)
               (if (llm-flows-state-user-verifier state)
                   (progn (funcall (llm-flows-state-user-verifier state) fsm event)
                          (list :user-verifier-waiting state))
                 ;; No verifier, so finalize things here.
                 (funcall (llm-flows-state-on-success state) event)
                 (list :end state))))
             (:user-verifier-waiting
              (:event
               (ignore fsm callback)
               (llm-flows-handle-verification state event)))
             (:end
              (:event
               (ignore callback fsm)
               (llm-flows-handle-end-state state event)))))

(defun llm-flows-handle-end-state (state event)
  (if (eq event :revise)
      (progn (when-let ((undof (llm-flows-state-on-undo state)))
               (funcall undof))
             (llm-flows-revise state))
    (list :end state)))

(defun llm-flows-handle-llm-response (fsm state event)
  "Hand the FSM receiving the LLM response.
FSM, STATE, and EVENT, and the return value is the same as in
`define-state'."
  (cl-incf (llm-flows-state-run-count state))
  (pcase (car event)
    (:success (fsm-send fsm (cdr event))
              (list (if (llm-flows-state-json-verifier state)
                        :json-verifier
                      :user-verifier) state))
    (:failed
     (funcall (llm-flows-state-on-error state) (cdr event))
     (list :end state))))

(defun llm-flows-revise (state)
  "Ask for revision and set the next state to be :chat."
  (setf (llm-flows-state-run-count state) 0)
  (llm-chat-prompt-append-response
   (llm-flows-state-prompt state)
   (read-from-minibuffer "Revision prompt: "))
  (list :chat state))

(defun llm-flows-handle-verification (state event)
  "Handle the FSM receiving a user verification event.
STATE is the current FSM state and EVENT is the event sent."
  (pcase (car event)
    ('accept
     (funcall (llm-flows-state-on-success state) (cdr event))
     (list :end state))
    ('repeat
     (list :chat state))
    ('revise
     (llm-flows-revise state))
    ('quit
     (list :end state))))

(defun llm-flows-json-extract-and-verify (fsm state text)
  "Extract and verify JSON from the result of the LLM.
Return is as expected from `fsm-define-state'."
  (if (> (llm-flows-state-run-count state) 5)
      (progn
        (fsm-send fsm "Could not extract json from LLM output after several tries.")
        (list :failed state))
    (let ((json (ignore-errors (llm-flows--extract-json text))))
      (if (and json (funcall (llm-flows-state-json-verifier state) json))
          (progn
            (fsm-send fsm json)
            (list :user-verifier state))
        (list :chat state)))))

(defun llm-flows-verify-result-diff (fsm before after)
  "Ask the user to accept the result of the LLM."
  (unless (string= before after)
    (let* ((name (llm-flows-state-name (fsm-get-state-data fsm)))
           (buf-begin (get-buffer-create (format "*%s diff before*" name)))
           (buf-end (get-buffer-create (format "*%s diff after*" name)))
           (orig-ediff-quit-hook ediff-quit-hook))
      (with-current-buffer buf-begin
        (erase-buffer)
        (insert before))
      (with-current-buffer buf-end
        (erase-buffer)
        (insert after))
      (add-hook 'ediff-quit-hook
                (lambda ()
                  (setq ediff-quit-hook orig-ediff-quit-hook)
                  (llm-flows-verify-query-user
                   fsm ""
                   (with-current-buffer buf-end
                     (buffer-substring-no-properties (point-min) (point-max)))
                   (lambda ()
                     (kill-buffer buf-begin)
                     (kill-buffer buf-end)))))
      (ediff-buffers buf-begin buf-end))))

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

(defun llm-flows-verify-query-user (fsm msg result &optional teardown-func)
  "Ask the user to verify via MSG.

FSM is the state machine to send the result to.

MSG will be suffixed with asking the user to accept the change,
and giving them the possibility to accept, revise or quit. MSG
should end in a period and a space, or a newline.

This is useful as the last expression in a verify function,
because it returns the correct symbols.

RESULT is the result that is being verified.

TEARDOWN-FUNC has no args and is used between reading the answer
and exiting."
  (let ((read-answer-short t))
    (fsm-send
     fsm
     (pcase
         (let ((answer (read-answer (format "%sAccept this change? " msg)
                                    '(("accept" ?a "accept the change")
                                      ("revise" ?r "ask llm for revision")
                                      ("quit" ?q "exit")))))
           (when teardown-func (funcall teardown-func))
           answer)
       ("accept" (cons 'accept result))
       ("revise" (cons 'revise result))
       ("quit" (cons 'quit result))))))

(provide 'llm-flows)

;;; llm-flows.el ends here
