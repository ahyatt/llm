;;; llm-test.el --- Unit tests for the llm module -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023-2025  Free Software Foundation, Inc.

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
(require 'llm-claude)
(require 'seq)

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
  (cond ((consp json-obj)
         (mapcan (lambda (x) (list (car x) (llm-test-normalize (cadr x))))
                 ;; We need to use seq-sort, due to sort incompatibilities in
                 ;; emacs 30.
                 (seq-sort (lambda (a b)
                             (string< (symbol-name (car a))
                                      (symbol-name (car b))))
                           (seq-partition json-obj 2))))
        ((vectorp json-obj)
         (vconcat
          (seq-sort (lambda (a b)
                      (string< (format "%s" a) (format "%s" b)))
                    (mapcar #'llm-test-normalize json-obj))))
        (t json-obj)))

(defconst llm-test-chat-requests-to-responses
  `((:name "Simple request"
           :prompt (lambda () (llm-make-chat-prompt "Hello world"))
           :openai-stream (:model "model"
                                  :messages [(:role "user" :content "Hello world")]
                                  :stream t)
           :openai (:model "model"
                           :messages [(:role "user" :content "Hello world")])

           :gemini (:contents [(:role "user" :parts [(:text "Hello world")])])
           :ollama (:model "model"
                           :messages [(:role "user" :content "Hello world")]
                           :stream :false)
           :claude (:model "model"
                           :max_tokens 4096
                           :messages [(:role "user" :content "Hello world")]
                           :stream :false)
           :claude-stream (:model "model"
                                  :max_tokens 4096
                                  :messages [(:role "user" :content "Hello world")]
                                  :stream t))
    (:name "Request with temperature"
           :prompt (lambda () (llm-make-chat-prompt "Hello world" :temperature 0.5))
           :openai-stream (:model "model"
                                  :messages [(:role "user" :content "Hello world")]
                                  :stream t
                                  :temperature 1.0)
           :gemini (:contents [(:role "user" :parts [(:text "Hello world")])]
                              :generationConfig (:temperature 1.0))
           :ollama (:model "model"
                           :messages [(:role "user" :content "Hello world")]
                           :options (:temperature 0.5)
                           :stream :false)
           :claude (:model "model"
                           :max_tokens 4096
                           :messages [(:role "user" :content "Hello world")]
                           :temperature 0.5
                           :stream :false))
    (:name "Request with context and examples"
           :prompt (lambda () (llm-make-chat-prompt "Hello world"
                                                    :context "context"
                                                    :examples (list (cons "input1" "output1")
                                                                    (cons "input2" "output2"))))
           :openai-stream (:model "model"
                                  :messages [(:role "system" :content "context\nExamples of how you should respond follow.\nUser: input1\nAssistant: output1\nUser: input2\nAssistant: output2")
                                             (:role "user" :content "Hello world")]
                                  :stream t)
           :gemini (:system_instruction
                    (:parts (:text "context\nExamples of how you should respond follow.\nUser: input1\nAssistant: output1\nUser: input2\nAssistant: output2"))
                    :contents [(:role "user" :parts [(:text "Hello world")])])
           :ollama (:model "model"
                           :messages [(:role "system" :content "context\nExamples of how you should respond follow.\nUser: input1\nAssistant: output1\nUser: input2\nAssistant: output2")
                                      (:role "user" :content "Hello world")]
                           :stream :false)
           :claude (:model "model"
                           :max_tokens 4096
                           :messages [(:role "user" :content "Hello world")]
                           :system "context\nHere are 2 examples of how to respond:\n\nUser: input1\nAssistant: output1\nUser: input2\nAssistant: output2"
                           :stream :false))
    (:name "Request with conversation"
           :prompt (lambda () (llm-make-chat-prompt '("Hello world" "Hello human" "I am user!")))
           :openai (:model "model"
                           :messages [(:role "user" :content "Hello world")
                                      (:role "assistant" :content "Hello human")
                                      (:role "user" :content "I am user!")])
           :gemini (:contents [(:role "user" :parts [(:text "Hello world")])
                               (:role "model" :parts [(:text "Hello human")])
                               (:role "user" :parts [(:text "I am user!")])])
           :ollama (:model "model"
                           :messages [(:role "user" :content "Hello world")
                                      (:role "assistant" :content "Hello human")
                                      (:role "user" :content "I am user!")]
                           :stream :false)
           :claude (:model "model"
                           :max_tokens 4096
                           :messages [(:role "user" :content "Hello world")
                                      (:role "assistant" :content "Hello human")
                                      (:role "user" :content "I am user!")]
                           :stream :false))
    (:name "Request with image"
           :prompt (lambda () (llm-make-chat-prompt
                               (make-llm-multipart
                                :parts (list "What is this?"
                                             (make-llm-media :mime-type "image/png"
                                                             :data "image data")))))
           :openai (:model "model"
                           :messages [(:role "user" :content [(:type "text" :text "What is this?")
                                                              (:type "image_url" :image_url (:url "data:image/png;base64,aW1hZ2UgZGF0YQ=="))])])
           :gemini (:contents
                    [(:role
                      "user"
                      :parts [(:text "What is this?")
                              (:inline_data (:mime_type "image/png"
                                                        :data "aW1hZ2UgZGF0YQ=="))])])
           :ollama (:model "model"
                           :messages [(:role "user"
                                             :content
                                             "What is this?"
                                             :images ["aW1hZ2UgZGF0YQ=="])]
                           :stream :false)
           :claude (:model "model"
                           :max_tokens 4096
                           :messages [(:role "user"
                                             :content
                                             [(:type "text" :text "What is this?")
                                              (:type "image" :source (:type "base64" :media_type "image/png" :data "aW1hZ2UgZGF0YQ=="))])]
                           :stream :false))
    (:name "Request with tools"
           :prompt (lambda () (llm-make-chat-prompt
                               "Hello world"
                               :tools (list (llm-make-tool
                                             :name "func"
                                             :description "desc"
                                             :args '((:name "arg1" :description "desc1" :type string)
                                                     (:name "arg2" :description "desc2" :type integer :optional t))))))
           :openai
           (:model "model"
                   :messages [(:role "user" :content "Hello world")]
                   :tools [(:type "function"
                                  :function
                                  (:name "func"
                                         :description "desc"
                                         :parameters
                                         (:type "object"
                                                :properties
                                                (:arg1 (:description "desc1" :type "string")
                                                       :arg2 (:description "desc2" :type "integer"))
                                                :required ["arg1"])))])
           :gemini
           (:contents [(:role "user" :parts [(:text "Hello world")])]
                      :tools [(:function_declarations
                               [(:name "func"
                                       :description "desc"
                                       :parameters
                                       (:type "object"
                                              :properties
                                              (:arg1 (:description "desc1" :type "string")
                                                     :arg2 (:description "desc2" :type "integer"))
                                              :required ["arg1"]))])])
           :ollama (:model "model"
                           :messages [(:role "user" :content "Hello world")]
                           :tools
                           [(:type "function"
                                   :function
                                   (:name "func"
                                          :description "desc"
                                          :parameters
                                          (:type "object"
                                                 :properties
                                                 (:arg1 (:description "desc1" :type "string")
                                                        :arg2 (:description "desc2" :type "integer"))
                                                 :required ["arg1"])))]
                           :stream :false)
           :claude (:model "model"
                           :max_tokens 4096
                           :messages [(:role "user" :content "Hello world")]
                           :tools
                           [(:name "func"
                                   :description "desc"
                                   :input_schema
                                   (:type "object"
                                          :properties
                                          (:arg1 (:description "desc1" :type "string")
                                                 :arg2 (:description "desc2" :type "integer"))
                                          :required ["arg1"]))]
                           :stream :false)))
  "A list of tests for `llm-provider-chat-request'.")

(ert-deftest llm-test-requests ()
  (dolist (test llm-test-chat-requests-to-responses)
    (dolist (provider '(openai gemini ollama claude))
      (dolist (variation (list nil 'stream))
        (when-let*
            ((plist-key (intern (format ":%s%s" provider (if variation "-stream" ""))))
             (expected-stream (plist-get test plist-key))
             (response (llm-provider-chat-request
                        (funcall (intern (format "make-llm-%s" provider))
                                 :chat-model "model")
                        (funcall (plist-get test :prompt))
                        (eq variation 'stream))))
          (ert-info ((format "Testing %s for model %s (%s)"
                             (plist-get test :name)
                             provider
                             (if variation "stream" "normal")))
            (should (equal (llm-test-normalize expected-stream)
                           (llm-test-normalize response)))))))))

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
  (should-not (member 'tool-use (llm-capabilities (make-llm-openai-compatible :chat-model "llama-3"))))
  (should (member 'tool-use (llm-capabilities (make-llm-openai-compatible :chat-model "llama-3.1"))))
  (should-not (member 'embeddings (llm-capabilities (make-llm-openai-compatible :chat-model "llama-3")))))

(ert-deftest llm-test-chat-token-limit-gemini ()
  (should (= 1048576 (llm-chat-token-limit (make-llm-gemini))))
  (should (= 1048576 (llm-chat-token-limit
                      (make-llm-gemini :chat-model "gemini-1.5-flash"))))
  (should (= 4096 (llm-chat-token-limit
                   (make-llm-vertex :chat-model "unknown")))))

(ert-deftest llm-test-capabilities-gemini ()
  (should-not (member 'tool-use (llm-capabilities (make-llm-gemini :chat-model "llama-3"))))
  (should (member 'tool-use (llm-capabilities (make-llm-gemini :chat-model "gemini-1.5-flash")))))

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

(ert-deftest llm-test-ollama-tool-use-capabilities ()
  ;; tests subject to change as models may get function calling
  (cl-flet ((has-fc (model)
              (member 'tool-use (llm-capabilities (make-llm-ollama :chat-model model)))))
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
