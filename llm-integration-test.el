;;; llm-intgration-test.el --- Integration tests for the llm module -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2024  Free Software Foundation, Inc.

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
;; This tests the `llm' module by running against a real backend. It is designed
;; to be as fast and accurate as possible, but since LLMs are not deterministic,
;; some flakiness may happen.

;;; Code:

(require 'llm)
(require 'ert)
(require 'seq)

(defconst llm-integration-test-chat-prompt
  "What is the capital of France?  Give me only one word, in English, with no punctuation."
  "A chat prompt to use for testing.")

(defconst llm-integration-test-chat-answer
  "Paris"
  "The correct answer to the chat prompt.")

(defun llm-integration-test-providers ()
  "Return a list of providers to test."
  (let ((providers))
    (when (getenv "OPENAI_KEY")
      (require 'llm-openai)
      (push (make-llm-openai :key (getenv "OPENAI_KEY")) providers))
    (when (getenv "ANTHROPIC_KEY")
      (require 'llm-claude)
      (push (make-llm-claude :key (getenv "ANTHROPIC_KEY")) providers))
    (when (getenv "GEMINI_KEY")
      (require 'llm-gemini)
      (push (make-llm-gemini :key (getenv "GEMINI_KEY")) providers))
    (when (getenv "VERTEX_PROJECT")
      (require 'llm-vertex)
      (push (make-llm-vertex :project (getenv "VERTEX_PROJECT")) providers))))

(ert-deftest llm-chat ()
  (dolist (provider (llm-integration-test-providers))
    (ert-info ((format "Using provider %s" (llm-name provider)))
      (should (equal
               (llm-chat
                provider
                (llm-make-chat-prompt llm-integration-test-chat-prompt))
               llm-integration-test-chat-answer)))))

(ert-deftest llm-chat-async ()
  (dolist (provider (llm-integration-test-providers))
    (ert-info ((format "Using provider %s" (llm-name provider)))
      (let ((result nil)
            (buf (current-buffer)))
        (llm-chat-async
         provider
         (llm-make-chat-prompt llm-integration-test-chat-prompt)
         (lambda (response)
           (should (eq (current-buffer) buf))
           (setq result response))
         (lambda (error)
           (error "Error: %s" error)))
        (while (null result)
          (sleep-for 0.1))
        (should (equal result llm-integration-test-chat-answer))))))

(ert-deftest llm-chat-streaming ()
  (dolist (provider (seq-filter
                     (lambda (provider)
                       (member 'streaming (llm-capabilities provider)))
                     (llm-integration-test-providers)))
    (ert-info ((format "Using provider %s" (llm-name provider)))
      (let ((streamed-result "")
            (returned-result nil)
            (buf (current-buffer))
            (start-time (current-time)))
        (llm-chat-streaming
         provider
         (llm-make-chat-prompt llm-integration-test-chat-prompt)
         (lambda (partial-response)
           (should (eq (current-buffer) buf))
           (setq streamed-result (concat streamed-result partial-response)))
         (lambda (response)
           (should (eq (current-buffer) buf))
           (setq returned-result response))
         (lambda (error)
           (error "Error: %s" error)))
        (while (and (null returned-result)
                    (time-less-p (time-subtract (current-time) start-time) 10))
          (sleep-for 0.1))
        (should (equal returned-result llm-integration-test-chat-answer))
        (should (equal streamed-result llm-integration-test-chat-answer))))))

(provide 'llm-integration-test)
