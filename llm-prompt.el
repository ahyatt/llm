;;; llm-prompt.el --- Utilities for LLM prompting -*- lexical-binding: t -*-

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
;; llm-prompt is a utility that makes it easy to define and provide data to
;; prompts for LLMs.  It provides a simple way to define prompts with a
;; template, and then fill in the template with data.  The data can be provided
;; as a value, a list, or a generator.  The core issue this solves is how to
;; provide different sequences of data that will be pulled until a certain
;; context size is reached.
;;
;; The selection of how to fill is random, based on tickets (see `llm-prompt'
;; for details), to avoid any biases with variable positioning or issues with
;; one variable repeatedly using up all the context window we want to use.
;;
;; To use this, create a prompt with `llm-defprompt', and then fill it with
;; `llm-prompt-fill'.
;;
;; For example, you could define a prompt like this:
;;
;; (defprompt 'my-facts "The user chatting with you is named {{user}} and the
;; following facts should be known: {{facts}}")
;;
;; When you want to call the LLM, you can use `llm-prompt-fill', which will fill
;; a fixed percentage of the context window `llm-prompt-default-max-pct' with
;; values to fill in the various variables in the templates.
;;
;; (llm-chat provider
;;   (llm-make-chat-prompt
;;      user-text
;;        :context (llm-prompt-fill 'my-facts :user my-user-name
;;                                  :facts #'user-fact-generator)))
;;
;; See the `llm-prompt-fill' for more information on what you can pass in.

(require 'generator)
(require 'cl-lib)
(require 'rx)
(require 'llm)

;;; Code:

(defgroup llm-prompt nil
  "Prompt construction and management for LLMs."
  :group 'llm)

(defcustom llm-prompt-default-max-pct 50
  "Default max percentage of context window to use for a prompt.
The minimum of this and `llm-prompt-default-max-tokens' will be
used.  For an example, at the time of this writing, using Claude
3.5 Sonnet will cost, at 50% tokens, $0.30 USD.

Using 100% or close to it is not recommended, as space is needed
for conversation, and token counting is not exact."
  :type 'integer
  :group 'llm-prompt)

(defcustom llm-prompt-default-max-tokens nil
  "The default maximum number of tokens to use for a prompt.
Set to nil to use `llm-prompt-default-max-pct' instead."
  :type 'integer
  :group 'llm-prompt)

(cl-defstruct llm-prompt piece text truncator)

(defvar llm-prompt-prompts (make-hash-table)
  "Stores prompts by symbol.
The prompts can be `prompt-piece' structs.  Alternatively, it can
be a list that defines other prompt pieces and how they fit
together (created via defprompt).")

(defmacro llm-defprompt (name text)
  "Define a prompt called NAME with TEXT.

TEXT is a string that can have placeholders.  The format of the
placeholder is {{argument<:tickets>}} where argument follows the
same rules and stylistic format as elisp symbols, and the
optional tickets part defines a number of tickets to assign to
this.  Each ticket defines a single element from a list of
elements, which are assigned in a random proportion to other
arguments with other tickets.  If not specified, it's assumed
that this will have as many tickets as the rest of all the other
arguments put together.  If no one specifies the number of
tickets, we will pull evenly (but randomly) into each of the
variables until we reach the desired context window size."
  (declare (indent defun))
  `(puthash (quote ,name) ,text llm-prompt-prompts))

(cl-defstruct llm-prompt-variable name tickets marker)
(cl-defstruct (llm-prompt-variable-full (:include llm-prompt-variable))
  generator)

(defconst llm-prompt-variable-placeholder-regex
  (rx (seq ?{ ?{ (group-n 1 (1+ (or alnum ?-)))
           (? (seq ?: (group-n 2 (1+ digit))))
           ?} ?})))

(defun llm-prompt-variables-to-markers ()
  "Convert variable placeholders in the current buffer to markers.
Return an alist of variables to their corresponding markers."
  (let ((results))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward llm-prompt-variable-placeholder-regex nil t)
        (let* ((name (intern (match-string 1)))
               (tickets (when (match-string 2)
                          (string-to-number (match-string 2))))
               (marker (make-marker)))
          (set-marker marker (match-beginning 0))
          (replace-match "")
          (push (make-llm-prompt-variable :name name :tickets tickets :marker marker) results))))
    (nreverse results)))

(defun llm-prompt--simple-var-p (var)
  "Return t if VAR is a simple variable, not a possible function.

Lists will be turned into generators, so they are not simple variables."
  (and (not (functionp var))
       (not (listp var))))

(iter-defun llm-prompt--select-tickets (vars)
  "Return generator that select tickets and calls generators in VARS.
TOTAL-SPECIFIED-TICKETS is the total number of tickets specified, not
counting the tickets not specified, which should equal this number."
  (let ((total (apply #'+ (mapcar (lambda (var)
                                    (llm-prompt-variable-tickets var))
                                  vars)))
        (using-vars (seq-copy vars)))
    (while using-vars
      (let ((r (random total)))
        (cl-loop for v in using-vars
                 with count = 0
                 do
                 (cl-incf count (llm-prompt-variable-tickets v))
                 until (> count r)
                 finally
                 (condition-case nil
                     (iter-yield (cons (llm-prompt-variable-name v)
                                       (iter-next (llm-prompt-variable-full-generator v))))
                   (iter-end-of-sequence
                    (progn
                      (setq using-vars (remove v using-vars)
                            total (- total (llm-prompt-variable-tickets v)))))))))))

(defun llm-prompt--ensure-iterator (var)
  "Return an iterator for VAR, if it's not already one.
If it is a list, it will be converted to a generator.  Any
function is assumed to be a generator.  The generator is then
executed with no arguments to return an iterator."
  (funcall
   (cond ((symbolp var) (symbol-function var))
         ((functionp var) var)
         (t (iter-lambda () (dolist (el var)
                              (iter-yield el)))))))

(defun llm-prompt--max-tokens (provider)
  "Return the maximum number of tokens to use for a prompt.
PROVIDER is the provider which will be used, and which has a
maximum number of tokens."
  (floor
   (min (or llm-prompt-default-max-tokens
            (llm-chat-token-limit provider))
        (* (/ llm-prompt-default-max-pct 100.0)
           (llm-chat-token-limit provider)))))

(defun llm-prompt-fill-text (text provider &rest keys)
  "Fill TEXT prompt, with the llm PROVIDER, values from KEYS.

PROVIER is an LLM provider.  KEYS is a plist of variables and
their values, either an actual value, or a list or function.  If
a function, it should return values via a generator."
  (with-temp-buffer
    (insert text)
    (let* ((final-vals nil)
           (vars (llm-prompt-variables-to-markers))
           (total-tokens (llm-count-tokens
                          provider (buffer-substring-no-properties (point-min) (point-max))))
           (keys-alist (mapcar (lambda (var)
                                 (cons (llm-prompt-variable-name var)
                                       (plist-get keys
                                                  (intern (format ":%s" (llm-prompt-variable-name var))))))
                               vars))
           (total-specified-tickets
            (let ((actual (apply
                           #'+
                           (mapcar (lambda (var)
                                     (if (llm-prompt--simple-var-p
                                          (assoc-default (llm-prompt-variable-name var)
                                                         keys-alist))
                                         0
                                       (or (llm-prompt-variable-tickets var) 0)))
                                   vars))))
              (if (= actual 0) 1 actual))))
      ;; First, we'll populate any variable that is passed in as a string,
      ;; integer, or float value.
      (mapc (lambda (var) (when (llm-prompt--simple-var-p
                                 (assoc-default (llm-prompt-variable-name var)
                                                keys-alist))
                            (let ((val (assoc-default (llm-prompt-variable-name var)
                                                      keys-alist)))
                              (push (cons (llm-prompt-variable-name var) val)
                                    final-vals)
                              (cl-incf total-tokens
                                       (llm-count-tokens provider
                                                         (format "%s" val))))))
            vars)
      (let ((ticket-gen (llm-prompt--select-tickets
                         (mapcan (lambda (var)
                                   (unless (llm-prompt--simple-var-p
                                            (assoc-default (llm-prompt-variable-name var)
                                                           keys-alist))
                                     (list (make-llm-prompt-variable-full
                                            :name (llm-prompt-variable-name var)
                                            :generator (llm-prompt--ensure-iterator
                                                        (assoc-default (llm-prompt-variable-name var)
                                                                       keys-alist))
                                            :tickets (or (llm-prompt-variable-tickets var)
                                                         total-specified-tickets)))))
                                 vars))))
        (condition-case nil
            (while (< total-tokens
                      (llm-prompt--max-tokens provider))
              (let* ((val-cons (iter-next ticket-gen))
                     (sval (format "%s" (cdr val-cons))))
                ;; Only add if there is space, otherwise we ignore this value.
                (when (<= (+ total-tokens (llm-count-tokens provider sval))
                          (* (/ llm-prompt-default-max-pct 100.0)
                             (llm-chat-token-limit provider)))
                  (cl-incf total-tokens (llm-count-tokens provider sval))
                  (if (assoc (car val-cons) final-vals)
                      (push sval (cdr (assoc (car val-cons) final-vals)))
                    (push (cons (car val-cons)
                                (list sval))
                          final-vals)))))
          (iter-end-of-sequence nil)))
      (cl-loop for (var-name . val) in final-vals
               do
               (goto-char
                (llm-prompt-variable-marker
                 (seq-find (lambda (e) (eq (llm-prompt-variable-name e)
                                           var-name))
                           vars)))
               (insert (format "%s" (if (listp val)
                                        (mapconcat (lambda (e)
                                                     (format "%s" e))
                                                   (reverse val) " ")
                                      val)))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun llm-prompt-fill (name provider &rest keys)
  "Get and fill the prompt for NAME given llm PROVIDER.
PROVIDER is an provider defined by the `llm' package.  KEYS is a
plist of variables and their values, either an actual value, or a
list or function.  If a function, it should return values via a
generator."
  (with-temp-buffer
    (let ((prompt-text (gethash name llm-prompt-prompts)))
      (unless prompt-text
        (error "Could not find prompt with name %s" name))
      (apply #'llm-prompt-fill-text prompt-text provider keys))))

(provide 'llm-prompt)

;;; llm-prompt.el ends here
