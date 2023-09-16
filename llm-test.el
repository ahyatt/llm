;;; llm-test.el --- Unit tests for the llm module -*- lexical-binding: t -*-

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
;; This tests just the code in the `llm' module, although it uses `llm-fake' as
;; well to do so. All individual providers are probably best tested in the
;; `llm-tester' module.

;;; Code:

(require 'llm)
(require 'llm-fake)
(require 'ert)

(ert-deftest llm-test-embedding ()
  (should-error (llm-embedding nil "Test"))
  ;; TODO: Test signals that are not errors, which ert doesn't seem to catch.
  (should-error (llm-embedding (make-llm-fake
                                :embedding-action-func
                                (lambda () (cons 'error "my message")))
                               "Test"))
  (should (equal
           [0.1 0.2 0.3]
           (llm-embedding (make-llm-fake :embedding-action-func (lambda () [0.1 0.2 0.3]))
                          "Test"))))

(ert-deftest llm-test-chat ()
  (should-error (llm-chat nil "Test"))
  (should-error (llm-chat-async nil "Test"))
  (should-error (llm-chat
                 (make-llm-fake
                  :chat-action-func (lambda () (cons 'error "my message")))
                 (make-llm-chat-prompt)))
  (should (equal
           "Response"
           (llm-chat (make-llm-fake :chat-action-func (lambda () "Response"))
                              (make-llm-chat-prompt)))))

;;; llm-test.el ends here
