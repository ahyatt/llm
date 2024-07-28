;;; llm-test.el --- Unit tests for the llm module -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023, 2024  Free Software Foundation, Inc.

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
(require 'llm-openai)
(require 'llm-gemini)
(require 'llm-vertex)
(require 'llm-ollama)
(require 'llm-gpt4all)

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

(ert-deftest llm-make-chat-prompt ()
  (should-error (llm-make-chat-prompt nil))
  (should-error (llm-make-chat-prompt '("a" "b")))
  (should (equal (llm-make-chat-prompt
                  '("a" "b" "c")
                  :temperature 0.2)
                 (make-llm-chat-prompt
                  :interactions (list (make-llm-chat-prompt-interaction
                                       :role 'user :content "a")
                                      (make-llm-chat-prompt-interaction
                                       :role 'assistant :content "b")
                                      (make-llm-chat-prompt-interaction
                                       :role 'user :content "c"))
                  :temperature 0.2)))
  (should (equal (llm-make-chat-prompt "a")
                 (make-llm-chat-prompt
                  :interactions (list (make-llm-chat-prompt-interaction
                                       :role 'user :content "a"))))))

(ert-deftest llm-test-chat-token-limit-openai ()
  (cl-flet* ((token-limit-for (model)
               (llm-chat-token-limit (make-llm-openai :chat-model model)))
             (should-have-token-limit (model limit)
               (ert-info ((format "Testing %s" model))
                 (should (equal limit (token-limit-for model))))))
    ;; From https://platform.openai.com/docs/models/gpt-3-5
    (should-have-token-limit "gpt-3.5-turbo-1106" 16385)
    (should-have-token-limit "gpt-3.5-turbo" 4096)
    (should-have-token-limit "gpt-3.5-turbo-16k" 16385)
    (should-have-token-limit "gpt-3.5-turbo-instruct" 4096)
    (should-have-token-limit "gpt-3.5-turbo-0613" 4096)
    (should-have-token-limit "gpt-3.5-turbo-16k-0613" 16385)
    (should-have-token-limit "gpt-3.5-turbo-0301" 4096)
    (should-have-token-limit "unknown" 4096)
    ;; From https://platform.openai.com/docs/models/gpt-4-and-gpt-4-turbo
    (should-have-token-limit "gpt-4-1106-preview" 128000)
    (should-have-token-limit "gpt-4-vision-preview" 128000)
    (should-have-token-limit "gpt-4" 8192)
    (should-have-token-limit "gpt-4-32k" 32768)
    (should-have-token-limit "gpt-4-0613" 8192)
    (should-have-token-limit "gpt-4-32k-0613" 32768)
    (should-have-token-limit "gpt-4o" 30000)
    (should-have-token-limit "gpt-4o-mini" 30000)
    (should-have-token-limit "unknown" 4096)))

(ert-deftest llm-test-chat-token-limit-gemini ()
  (should (= 30720 (llm-chat-token-limit (make-llm-gemini))))
  (should (= 12288 (llm-chat-token-limit
                    (make-llm-gemini :chat-model "gemini-pro-vision"))))
  (should (= 1048576 (llm-chat-token-limit
                      (make-llm-gemini :chat-model "gemini-1.5-flash"))))
  (should (= 2048 (llm-chat-token-limit
                   (make-llm-vertex :chat-model "unknown")))))

(ert-deftest llm-test-chat-token-limit-vertex ()
  (should (= 30720 (llm-chat-token-limit (make-llm-vertex))))
  (should (= 12288 (llm-chat-token-limit
                    (make-llm-vertex :chat-model "gemini-pro-vision"))))
  (should (= 1048576 (llm-chat-token-limit
                      (make-llm-gemini :chat-model "gemini-1.5-flash"))))
  (should (= 2048 (llm-chat-token-limit
                   (make-llm-vertex :chat-model "unknown")))))

(ert-deftest llm-test-chat-token-limit-ollama ()
  ;; The code is straightforward, so no need to test all the models.
  (should (= 8192 (llm-chat-token-limit
                   (make-llm-ollama :chat-model "mistral:latest"))))
  (should (= 131072 (llm-chat-token-limit
                     (make-llm-vertex :chat-model "llama3.1-8b"))))
  (should (= 2048 (llm-chat-token-limit
                   (make-llm-ollama :chat-model "unknown")))))

(ert-deftest llm-test-chat-token-limit-gpt4all ()
  ;; The code is straightforward, so no need to test all the models.
  (should (= 8192 (llm-chat-token-limit
                   (make-llm-gpt4all :chat-model "Mistral")))))

(ert-deftest llm-test-ollama-function-calling-capabilities ()
  ;; tests subject to change as models may get function calling
  (cl-flet ((has-fc (model)
              (member 'function-calls (llm-capabilities (make-llm-ollama :chat-model model)))))
    (should (has-fc "llama3.1"))
    (should (has-fc "llama3.1:8b-instruct-q8_0"))
    (should (has-fc "mistral"))
    (should-not (has-fc "gemma"))
    (should-not (has-fc "gemma2"))
    (should-not (has-fc "llama2"))
    (should-not (has-fc "llama"))
    (should-not (has-fc "unknown"))))

(ert-deftest llm-test-ollama-embedding-capabilities ()
  ;; tests subject to change as models may get function calling
  (cl-flet ((has-emb (model)
              (member 'embeddings
                      (llm-capabilities (make-llm-ollama :embedding-model model
                                                         :chat-model "mistral")))))
    (should-not (has-emb "llama3.1"))
    (should-not (has-emb "mistral"))
    (should (has-emb "nomic-embed-text"))
    (should (has-emb "mxbai-embed-large"))
    (should-not (has-emb "mxbai-embed-small"))
    (should-not (has-emb "unknown"))
    (should-not (has-emb nil))))

(provide 'llm-test)
;;; llm-test.el ends here
