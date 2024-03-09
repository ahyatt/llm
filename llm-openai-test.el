;;; llm-openai-test.el --- Tests for llm-openai -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

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

;; Tests for llm-openai.

;;; Code:

(require 'ert)
(require 'llm)
(require 'llm-openai)

(defvar llm-openai-test-token
  (auth-source-pick-first-password :host "openai.com" :user "ellama"))

(defvar llm-openai-test-provider
  (make-llm-openai :key llm-openai-test-token))

(defvar llm-openai-test-prompt
  (llm-make-simple-chat-prompt "Hello"))

(ert-deftest llm-openai-test-llm-chat ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key llm-openai-test-token))
        (prompt (llm-make-simple-chat-prompt "Hello")))
    (setf (llm-chat-prompt-temperature prompt) 0.01)
    (should (equal "Hello! How can I assist you today?"
                   (llm-chat provider prompt)))))

(ert-deftest llm-openai-test-llm-chat-invalid-key ()
  (condition-case error
      (let ((llm-warn-on-nonfree nil)
            (provider (make-llm-openai :key "invalid"))
            (prompt (llm-make-simple-chat-prompt "Hello")))
        (llm-chat provider prompt)
        (should nil))
    (error
     (should (equal 'error (car error)))
     (should (string-match "LLM request failed with code 401" (cadr error))))))

(ert-deftest llm-openai-test-llm-chat-async ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key llm-openai-test-token))
        (prompt (llm-make-simple-chat-prompt "Hello"))
        (errors) (responses))
    (setf (llm-chat-prompt-temperature prompt) 0.01)
    (llm-chat-async provider prompt
                    (lambda (response)
                      (push response responses))
                    (lambda (error description)
                      (push (list error description) errors)))
    (while (not (or responses errors))
      (sleep-for 0.5))
    (should (zerop (length errors)))
    (should (equal 1 (length responses)))
    (seq-doseq (response responses)
      (should (equal "Hello! How can I assist you today?" response)))))

(ert-deftest llm-openai-test-llm-chat-async-invalid-key ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key "invalid"))
        (prompt (llm-make-simple-chat-prompt "Hello"))
        (errors) (responses))
    (llm-chat-async provider prompt
                    (lambda (response)
                      (push response responses))
                    (lambda (error description)
                      (push (list error description) errors)))
    (while (not (or responses errors))
      (sleep-for 0.5))
    (should (equal 1 (length errors)))
    (seq-doseq (pair errors)
      (should (equal 'error (car pair)))
      (should (string-match "Problem calling Open AI: invalid_request_error"
                            (cadr pair))))
    (should (equal 0 (length responses)))))

(ert-deftest llm-openai-test-llm-embedding ()
  (let* ((llm-warn-on-nonfree nil)
         (provider (make-llm-openai :key llm-openai-test-token))
         (embedding (llm-embedding provider "Hello")))
    (should (vectorp embedding))
    (should (seq-every-p #'floatp embedding))))

(ert-deftest llm-openai-test-llm-embedding-invalid-key ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key "invalid"))
        (embeddings) (errors))
    (llm-embedding-async provider "Hello"
                         (lambda (embedding)
                           (push embedding embeddings))
                         (lambda (error description)
                           (push (list error description) errors)))
    (while (not (or embeddings errors))
      (sleep-for 0.5))
    (should (equal 1 (length errors)))))

(ert-deftest llm-openai-test-llm-embedding-async ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key llm-openai-test-token))
        (embeddings) (errors))
    (llm-embedding-async provider "Hello"
                         (lambda (embedding)
                           (push embedding embeddings))
                         (lambda (error description)
                           (push (list error description) errors)))
    (while (not (or embeddings errors))
      (sleep-for 0.5))
    (should (zerop (length errors)))
    (should (equal 1 (length embeddings)))
    (seq-doseq (embedding embeddings)
      (should (vectorp embedding))
      (should (seq-every-p #'floatp embedding)))))

(ert-deftest llm-openai-test-llm-embedding-async-invalid-key ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key "invalid"))
        (embeddings) (errors))
    (llm-embedding-async provider "Hello"
                         (lambda (embedding)
                           (push embedding embeddings))
                         (lambda (error description)
                           (push (list error description) errors)))
    (while (not (or embeddings errors))
      (sleep-for 0.5))
    (should (zerop (length embeddings)))
    (should (equal 1 (length errors)))
    (should (equal 1 (length errors)))
    (seq-doseq (error errors)
      (should (equal 'error (car error)))
      (should (string-match "Problem calling Open AI: invalid_request_error"
                            (cadr error))))))

(ert-deftest llm-openai-test-llm-chat-streaming ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key llm-openai-test-token))
        (prompt (llm-make-simple-chat-prompt "Hello"))
        (errors) (partial-responses) (responses))
    (setf (llm-chat-prompt-temperature prompt) 0.01)
    (llm-chat-streaming provider prompt
                        (lambda (response)
                          (push response partial-responses))
                        (lambda (response)
                          (push response responses))
                        (lambda (error description)
                          (push (list error description) errors)))
    (while (not (or responses errors))
      (sleep-for 0.5))
    (should (zerop (length errors)))
    (should (equal 1 (length responses)))
    (seq-doseq (response responses)
      (should (equal "Hello! How can I assist you today?" response)))
    (should (> (length partial-responses) 1))
    (seq-doseq (partial-response (reverse partial-responses))
      (should (string-match partial-response (car responses))))))

(ert-deftest llm-openai-test-llm-chat-streaming-invalid-key ()
  (let ((llm-warn-on-nonfree nil)
        (provider (make-llm-openai :key "invalid"))
        (prompt (llm-make-simple-chat-prompt "Hello"))
        (errors) (partial-responses) (responses))
    (llm-chat-streaming provider prompt
                        (lambda (response)
                          (push response partial-responses))
                        (lambda (response)
                          (push response responses))
                        (lambda (error description)
                          (push (list error description) errors)))
    (while (not (or responses errors))
      (sleep-for 0.5))
    (should (equal 1 (length errors)))
    (seq-doseq (error errors)
      (should (equal 'error (car error)))
      (should (string-match "Problem calling Open AI: invalid_request_error"
                            (cadr error))))
    (should (zerop (length partial-responses)))
    (should (zerop (length responses)))))

(provide 'llm-openai-test)
;;; llm-openai-test.el ends here
