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

(defun llm-test-normalize (json-obj)
  "Normalize JSON-OBJ for comparison."
  (cond ((plistp json-obj)
         (mapcan (lambda (x) (list (car x) (llm-test-normalize (cadr x))))
                 (sort (seq-partition json-obj 2)
                       (lambda (a b)
                         (string< (symbol-name (car a))
                                  (symbol-name (car b)))))))
        ((vectorp json-obj)
         (vconcat (mapcar #'llm-test-normalize json-obj)))
        (t json-obj)))

(defconst llm-test-chat-requests-to-responses
  `((:name "Simple request"
           :prompt ,(llm-make-chat-prompt "Hello world")
           :openai-stream (:model "model"
                                  :messages [(:role user :content "Hello world")]
                                  :stream t)
           :gemini-stream (:contents [(:role user :parts [(:text "Hello world")])]))
    (:name "Request with temperature"
           :prompt ,(llm-make-chat-prompt "Hello world" :temperature 0.5)
           :openai-stream (:model "model"
                                  :messages [(:role user :content "Hello world")]
                                  :stream t
                                  :temperature 1.0)
           :gemini-stream (:contents [(:role user :parts [(:text "Hello world")])]
                                     :generationConfig (:temperature 1.0)))
    (:name "Request with context and examples"
           :prompt ,(llm-make-chat-prompt "Hello world"
                                          :context "context"
                                          :examples (list (cons "input1" "output1")
                                                          (cons "input2" "output2")))
           :openai-stream (:model "model"
                                  :messages [(:role system :content "context\nExamples of how you should respond follow.\nUser: input1\nAssistant: output1\nUser: input2\nAssistant: output2")
                                             (:role user :content "Hello world")]
                                  :stream t)
           :gemini-stream (:system_instruction
                           (:parts (:text "context\nExamples of how you should respond follow.\nUser: input1\nAssistant: output1\nUser: input2\nAssistant: output2"))
                           :contents [(:role user :parts [(:text "Hello world")])]))
    (:name "Request with conversation"
           :prompt ,(llm-make-chat-prompt '("Hello world" "Hello human" "I am user!"))
           :openai-stream (:model "model"
                                  :messages [(:role user :content "Hello world")
                                             (:role assistant :content "Hello human")
                                             (:role user :content "I am user!")]
                                  :stream t)
           :gemini-stream (:contents [(:role user :parts [(:text "Hello world")])
                                      (:role model :parts [(:text "Hello human")])
                                      (:role user :parts [(:text "I am user!")])]))
    (:name "Request with image"
           :prompt ,(llm-make-chat-prompt
                     (make-llm-multipart
                      :parts (list "What is this?"
                                   (make-llm-media :mime-type "image/png"
                                                   :data (base64-encode-string "image data")))))
           :openai-stream (:model "model"
                                  :messages [(:role user :content [(:type "text" :text "What is this?")
                                                                   (:type "image_url" :image_url (:url "data:image/png;base64,YVcxaFoyVWdaR0YwWVE9PQ=="))])]
                                  :stream t)
           :gemini-stream (:contents
                           [(:role
                             user
                             :parts [(:text "What is this?")
                                     (:inline_data (:mime_type "image/png"
                                                               :data "YVcxaFoyVWdaR0YwWVE9PQ=="))])]))
    (:name "Request with tools"
           :prompt ,(llm-make-chat-prompt
                     "Hello world"
                     :tools (list (make-llm-tool-function
                                   :name "func"
                                   :description "desc"
                                   :args '((:name "arg1" :description "desc1" :type "string" :required t)
                                           (:name "arg2" :description "desc2" :type "integer")))))
           :openai-stream
           (:model "model"
                   :messages [(:role user :content "Hello world")]
                   :tools [(:type function
                                  :function
                                  (:name "func"
                                         :description "desc"
                                         :parameters
                                         (:type object
                                                :properties
                                                (:arg1 (:description "desc1" :type string)
                                                       :arg2 (:description "desc2" :type integer))
                                                :required [arg1])))]
                   :stream t)
           :gemini-stream
           (:contents [(:role user :parts [(:text "Hello world")])]
                      :tools [(:function_declarations
                               [(:name "func"
                                       :description "desc"
                                       :parameters
                                       (:type object
                                              :properties
                                              (:arg1 (:description "desc1" :type string)
                                                     :arg2 (:description "desc2" :type integer))
                                              :required [arg1]))])])))
  "A list of tests for `llm-provider-chat-request'.")

(ert-deftest llm-test-requests ()
  (let ((openai-model (make-llm-openai :chat-model "model"))
        (gemini-model (make-llm-gemini)))
    (dolist (test llm-test-chat-requests-to-responses)
      (ert-info ((format "Testing %s" (plist-get test :name)))
        (when-let* ((expected-stream (plist-get test :openai-stream)))
          (should (equal (llm-test-normalize expected-stream)
                         (llm-test-normalize (llm-provider-chat-request
                                              openai-model
                                              (plist-get test :prompt)
                                              t)))))
        (when-let* ((expected-stream (plist-get test :gemini-stream)))
          (should (equal (llm-test-normalize expected-stream)
                         (llm-test-normalize (llm-provider-chat-request
                                              gemini-model
                                              (plist-get test :prompt)
                                              t)))))))))

(ert-deftest llm-test-chat-token-limit-openai ()
  (cl-flet* ((token-limit-for (model)
                              (llm-chat-token-limit (make-llm-openai :chat-model model)))
             (should-have-token-limit (model limit)
                                      (ert-info ((format "Testing %s" model))
                                        (should (equal limit (token-limit-for model))))))
    ;; From https://platform.openai.com/docs/models/gpt-3-5
    (should-have-token-limit "gpt-3.5-turbo" 16385)
    (should-have-token-limit "gpt-3.5-turbo-instruct" 4096)
    (should-have-token-limit "unknown" 4096)
    ;; From https://platform.openai.com/docs/models/gpt-4-and-gpt-4-turbo
    (should-have-token-limit "gpt-4" 8192)
    (should-have-token-limit "gpt-4-0613" 8192)
    ;; I couldn't find documentation on this, but the token limit is actually
    ;; 30k instead of 128k for most customers.
    (should-have-token-limit "gpt-4o" 30000)
    (should-have-token-limit "gpt-4o-mini" 30000)
    (should-have-token-limit "unknown" 4096)))

(ert-deftest llm-test-capabilities-openai-compatible ()
  (should-not (member 'function-calls (llm-capabilities (make-llm-openai-compatible :chat-model "llama-3"))))
  (should (member 'function-calls (llm-capabilities (make-llm-openai-compatible :chat-model "llama-3.1"))))
  (should-not (member 'embeddings (llm-capabilities (make-llm-openai-compatible :chat-model "llama-3")))))

(ert-deftest llm-test-chat-token-limit-gemini ()
  (should (= 2097152 (llm-chat-token-limit (make-llm-gemini))))
  (should (= 1048576 (llm-chat-token-limit
                      (make-llm-gemini :chat-model "gemini-1.5-flash"))))
  (should (= 4096 (llm-chat-token-limit
                   (make-llm-vertex :chat-model "unknown")))))

(ert-deftest llm-test-capabilities-gemini ()
  (should-not (member 'function-calls (llm-capabilities (make-llm-gemini :chat-model "llama-3"))))
  (should (member 'function-calls (llm-capabilities (make-llm-gemini :chat-model "gemini-1.5-flash")))))

(ert-deftest llm-test-chat-token-limit-vertex ()
  (should (= 2097152 (llm-chat-token-limit (make-llm-vertex))))
  (should (= 1048576 (llm-chat-token-limit
                      (make-llm-gemini :chat-model "gemini-1.5-flash"))))
  (should (= 4096 (llm-chat-token-limit
                   (make-llm-vertex :chat-model "unknown")))))

(ert-deftest llm-test-chat-token-limit-ollama ()
  ;; The code is straightforward, so no need to test all the models.
  (should (= 8192 (llm-chat-token-limit
                   (make-llm-ollama :chat-model "mistral:latest"))))
  (should (= 128000 (llm-chat-token-limit
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
