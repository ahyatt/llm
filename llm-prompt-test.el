;;; llm-prompt-test.el --- Tests for LLM prompting -*- lexical-binding: t -*-

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

(require 'generator)
(require 'ert)
(require 'llm)
(require 'llm-prompt)

;;; Code:

(ert-deftest llm-prompt-variables-to-markers ()
  (with-temp-buffer
    (insert "Text {{var-1}}, {{var-2:30}}")
    (let ((variables (llm-prompt-variables-to-markers))
          (m1 (make-marker))
          (m2 (make-marker)))
      (set-marker m1 6)
      (set-marker m2 17)
      (should (equal (buffer-string) "Text , "))
      (should (equal variables
                     (list (make-llm-prompt-variable :name 'var-1 :marker m1)
                           (make-llm-prompt-variable :name 'var-2 :tickets 30 :marker m2))))
      (goto-char (marker-position (llm-prompt-variable-marker (cl-first variables))))
      (insert "INSERTED")
      (should (equal (buffer-string) "Text INSERTED, ")))))

(defun approx-equal (a b &optional epsilon)
  "Return t if A and B are approximately equal within EPSILON.
If EPSILON is not provided, it defaults to a small value, but big
enough so that we don't have to run a large number of iterations
to converge."
  (let ((epsilon (or epsilon 1e-2)))
    (< (abs (- a b)) epsilon)))

(ert-deftest llm-prompt--select-tickets ()
  (let* ((a-count 0)
         (b-count 0)
         (c-count 0)
         ;; The total is 60, so var-1 gets 1/6 of the total, var-2 gets 1/3, and
         ;; var-3 gets 1/2.  var-4 just has one value, so even though they have
         ;; 30, it shouldn't make any difference.
         (vars (list (make-llm-prompt-variable-full
                      :name 'var-1 :tickets 10
                      :generator (funcall
                                  (iter-lambda ()
                                    (while t (iter-yield 'a)))))
                     (make-llm-prompt-variable-full
                      :name 'var-2 :tickets 20
                      :generator (funcall
                                  (iter-lambda ()
                                    (while t (iter-yield 'b)))))
                     (make-llm-prompt-variable-full
                      :name 'var-3 :tickets 30
                      :generator (funcall
                                  (iter-lambda ()
                                    (while t (iter-yield 'c)))))
                     (make-llm-prompt-variable-full
                      :name 'var-4 :tickets 30
                      :generator (funcall (iter-lambda ()
                                            (iter-yield 'd))))))
         (selector (llm-prompt--select-tickets vars))
         (iters 20000))
    (dotimes (_ iters)
      (let ((result (iter-next selector)))
        (pcase (cdr result)
          ('a (cl-incf a-count)
              (should (eq (car result) 'var-1)))
          ('b (cl-incf b-count)
              (should (eq (car result) 'var-2)))
          ('c (cl-incf c-count)
              (should (eq (car result) 'var-3)))))
      )
    ;; If these are good, no need to test the final value.
    (should (approx-equal (/ (float a-count) iters) (/ 1.0 6.0)))
    (should (approx-equal (/ (float b-count) iters) (/ 1.0 3.0)))))

(ert-deftest llm-prompt--select-tickets-from-one ()
  (let* ((result)
         (vars (list (make-llm-prompt-variable-full
                      :name 'var-1 :tickets 10
                      :generator (funcall
                                  (iter-lambda ()
                                    (while t (iter-yield 'a)))))))
         (selector (llm-prompt--select-tickets vars)))
    (push (cdr (iter-next selector)) result)
    (push (cdr (iter-next selector)) result)
    (push (cdr (iter-next selector)) result)
    (should (equal result '(a a a)))))

(ert-deftest llm-prompt--select-tickets-running-out ()
  (let* ((vars (list (make-llm-prompt-variable-full
                      :name 'var :tickets 10
                      :generator (funcall
                                  (iter-lambda ()
                                    (iter-yield 'a))))))
         (selector (llm-prompt--select-tickets vars)))
    (should (equal (cdr (iter-next selector)) 'a))
    (condition-case nil
        (progn (iter-next selector)
               (ert-fail "Should have signaled with 'iter-end-of-sequence"))
      (iter-end-of-sequence nil))))

(iter-defun llm-prompt-test-iterator () (iter-yield 1))

(ert-deftest llm-prompt--ensure-iterator ()
  (dolist (e (list '(1) #'llm-prompt-test-iterator (iter-lambda () (iter-yield 1))))
    (should (equal 1 (iter-next (llm-prompt--ensure-iterator e))))))

(cl-defstruct prompt-test-llm)
(cl-defmethod llm-chat-token-limit ((_ prompt-test-llm))
  20)
(cl-defmethod llm-count-tokens ((_ prompt-test-llm) string)
  (length string))

(ert-deftest llm-prompt-fill ()
  (let ((llm-prompt-prompts (make-hash-table))
        (llm-prompt-default-max-pct 50))
    (puthash 'test "({{var1}}) ({{var2}})" llm-prompt-prompts)
    (should-error (llm-prompt-fill 'notfound (make-prompt-test-llm)))
    (let* ((result (llm-prompt-fill
                    'test (make-prompt-test-llm)
                    :var1 '(1 2 3 4)
                    :var2 '(a b c d)))
           (var1-cons (read-from-string result))
           (var1 (car var1-cons))
           (var2 (car (read-from-string result (cdr var1-cons)))))
      ;; We don't know what we're going to get because of the randomness in
      ;; filling.  So let's test what we know should be true: we aren't going to
      ;; be over 50% of the token limit, 26, and since tokens are equivalent to
      ;; length in our provider that should mean we get 5 total things filled,
      ;; something like "(1 2 3) (a b)", which is 13, but we don't count
      ;; separators for efficiency reasons, so it really is counted as 10.  and
      ;; at least one of each of the variables, meaning that `1' and `a' should
      ;; both appear.
      (should (member 1 var1))
      (should (member 'a var2))
      (should (= 5 (+ (length var1) (length var2)))))))

(ert-deftest llm-prompt-fill-with-compiled-function ()
  (should (equal
           (llm-prompt-fill-text "({{var1}})"
                                 (make-prompt-test-llm)
                                 :var1 (byte-compile (iter-lambda () (iter-yield 'a))))
           "(a)")))

(ert-deftest llm-prompt-fill-size-limit-after-initial-fill ()
  ;; After filling the intial var1 from a string, we don't have enough tokens to
  ;; do any more filling.
  (should (equal
           "(A long, long string.) ()"
           (llm-prompt-fill-text "({{var1}}) ({{var2}})"
                                 (make-prompt-test-llm)
                                 :var1 "A long, long string."
                                 :var2 '(a b c d)))))

(ert-deftest llm-prompt-reject-oversized ()
  ;; We should reject any fill that is over the token limit.
  ;; But we still need to continue filling after the rejection.
  (should (equal "(a b c d)"
                 (llm-prompt-fill-text
                  "({{var1}})"
                  (make-prompt-test-llm)
                  :var1 '("this is a completely oversized item"
                          "a" "b" "c" "d")))))

(ert-deftest llm-prompt--max-tokens ()
  (cl-flet ((should-have-max-tokens (expected max-pct max-tokens)
              (let ((llm-prompt-default-max-pct max-pct)
                    (llm-prompt-default-max-tokens max-tokens))
                (should (equal expected (llm-prompt--max-tokens
                                         (make-prompt-test-llm)))))))
    ;; The test LLM has a 20 token limit
    (should-have-max-tokens 10 50 nil)
    (should-have-max-tokens 20 100 nil)
    (should-have-max-tokens 5 50 5)
    (should-have-max-tokens 10 50 10)
    (should-have-max-tokens 10 50 20)))

(provide 'llm-prompt-test)

;;; llm-prompt-test.el ends here
