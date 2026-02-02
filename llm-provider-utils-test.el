;;; llm-provider-utils-test.el --- Tests for llm-provider-utils -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023-2025  Free Software Foundation, Inc.

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
;; This file provides functions to help build providers. It should only be used
;; by modules implementing an LLM provider.

;;; Code:

(require 'cl-macs)
(require 'llm-provider-utils)
(require 'llm)

(ert-deftest llm-provider-utils-openai-arguments ()
  (let* ((args
          (list
           ;; A required string arg
           '(:name "location"
                   :type string
                   :description "The city and state, e.g. San Francisco, CA")
           ;; A string arg with an name
           '(:name "unit"
                   :type string
                   :description "The unit of temperature, either 'celsius' or 'fahrenheit'"
                   :enum ["celsius" "fahrenheit"]
                   :optional t)
           '(:name "postal_codes"
                   :type array
                   :description "Specific postal codes"
                   :items (:type string)
                   :optional t)))
         (result (llm-provider-utils-openai-arguments args))
         (expected
          '(:type "object"
                  :properties
                  (:location
                   (:type "string"
                          :description "The city and state, e.g. San Francisco, CA")
                   :unit
                   (:type "string"
                          :description "The unit of temperature, either 'celsius' or 'fahrenheit'"
                          :enum ["celsius" "fahrenheit"])
                   :postal_codes (:type "array"
                                        :description "Specific postal codes"
                                        :items (:type "string")))
                  :required ["location"])))
    (should (equal result expected))))

(ert-deftest llm-provider-utils-convert-to-serializable ()
  (should (equal (llm-provider-utils-convert-to-serializable '(:a 1 :b 2))
                 '(:a 1 :b 2)))
  (should (equal (llm-provider-utils-convert-to-serializable '(:a "1" :b foo))
                 '(:a "1" :b "foo")))
  (should (equal (llm-provider-utils-convert-to-serializable '(:inner '(:a foo :b bar)))
                 '(:inner '(:a "foo" :b "bar")))))

(ert-deftest llm-provider-utils-append-to-prompt ()
  (let ((prompt (llm-make-chat-prompt "Prompt")))
    (llm-provider-utils-append-to-prompt prompt '(:a 1 :b :json-false)
                                         (list
                                          (make-llm-chat-prompt-tool-result
                                           :tool-name "tool"
                                           :result :json-false)))
    (should (equal (nth 1 (llm-chat-prompt-interactions prompt))
                   (make-llm-chat-prompt-interaction
                    :role 'tool-results
                    :content "(:a 1 :b nil)"
                    :tool-results (list
                                   (make-llm-chat-prompt-tool-result
                                    :tool-name "tool"
                                    :result :false)))))))

(ert-deftest llm-provider-utils-combine-to-system-prompt ()
  (let* ((interaction1 (make-llm-chat-prompt-interaction :role 'user :content "Hello"))
         (example1 (cons "Request 1" "Response 1"))
         (example2 (cons "Request 2" "Response 2"))
         (prompt-for-first-request
          (make-llm-chat-prompt
           :context "Example context"
           :interactions (list (copy-llm-chat-prompt-interaction interaction1))
           :examples (list example1 example2)))
         (prompt-with-existing-system-prompt
          (make-llm-chat-prompt
           :context "Example context"
           :interactions (list
                          (make-llm-chat-prompt-interaction :role 'system :content "Existing system prompt.")
                          (copy-llm-chat-prompt-interaction interaction1))
           :examples (list example1 example2))))
    (llm-provider-utils-combine-to-system-prompt prompt-for-first-request)
    (should (= 2 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should (equal "Example context\nHere are 2 examples of how to respond:\n\nUser: Request 1\nAssistant: Response 1\nUser: Request 2\nAssistant: Response 2"
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-for-first-request)))))
    (should (equal "Hello" (llm-chat-prompt-interaction-content (nth 1 (llm-chat-prompt-interactions prompt-for-first-request)))))
    (should-not (llm-chat-prompt-context prompt-for-first-request))
    (should-not (llm-chat-prompt-examples prompt-for-first-request))

    ;; On the request with the existing system prompt, it should append the new
    ;; text to the existing system prompt.
    (llm-provider-utils-combine-to-system-prompt prompt-with-existing-system-prompt)
    (should (= 2 (length (llm-chat-prompt-interactions prompt-with-existing-system-prompt))))
    (should (equal "Existing system prompt.\nExample context\nHere are 2 examples of how to respond:\n\nUser: Request 1\nAssistant: Response 1\nUser: Request 2\nAssistant: Response 2"
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-with-existing-system-prompt)))))))

(ert-deftest llm-provider-utils-combine-to-user-prompt ()
  (let* ((interaction1 (make-llm-chat-prompt-interaction :role 'user :content "Hello"))
         (example1 (cons "Request 1" "Response 1"))
         (example2 (cons "Request 2" "Response 2"))
         (prompt-for-first-request
          (make-llm-chat-prompt
           :context "Example context"
           :interactions (list (copy-llm-chat-prompt-interaction interaction1))
           :examples (list example1 example2))))
    ;; In the first request, the system prompt should be prepended to the user request.
    (llm-provider-utils-combine-to-user-prompt prompt-for-first-request)
    (should (= 1 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should-not (llm-chat-prompt-context prompt-for-first-request))
    (should-not (llm-chat-prompt-examples prompt-for-first-request))
    (should (equal "Example context\nHere are 2 examples of how to respond:\n\nUser: Request 1\nAssistant: Response 1\nUser: Request 2\nAssistant: Response 2\nHello"
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-for-first-request)))))))

(ert-deftest llm-provider-utils-collapse-history ()
  (let* ((interaction1 (make-llm-chat-prompt-interaction :role 'user :content "Hello"))
         (interaction2 (make-llm-chat-prompt-interaction :role 'assistant :content "Hi! How can I assist you?"))
         (interaction3 (make-llm-chat-prompt-interaction :role 'assistant :content "Earl Grey, hot."))
         (prompt-for-first-request
          (make-llm-chat-prompt
           :interactions (list (copy-llm-chat-prompt-interaction interaction1))))
         (prompt-for-second-request
          (make-llm-chat-prompt
           :interactions (list (copy-llm-chat-prompt-interaction interaction1)
                               (copy-llm-chat-prompt-interaction interaction2)
                               (copy-llm-chat-prompt-interaction interaction3)))))
    ;; In the first request, there's no history, so nothing should be done.
    (llm-provider-utils-collapse-history prompt-for-first-request)
    (should (= 1 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should (equal interaction1 (nth 0 (llm-chat-prompt-interactions prompt-for-first-request))))

    ;; In the second request we should have the history prepended.
    (llm-provider-utils-collapse-history prompt-for-second-request)
    (should (= 1 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should (equal "Previous interactions:\n\nUser: Hello\nAssistant: Hi! How can I assist you?\n\nThe current conversation follows:\n\nEarl Grey, hot."
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-for-second-request)))))))

(ert-deftest llm-provider-utils-streaming-accumulate ()
  (should (equal 3 (llm-provider-utils-streaming-accumulate 1 2)))
  (should (equal "foobar" (llm-provider-utils-streaming-accumulate "foo" "bar")))
  (should (equal [1 2 3] (llm-provider-utils-streaming-accumulate [1] [2 3])))
  (should (equal '(1 2 3) (llm-provider-utils-streaming-accumulate '(1) '(2 3))))
  (should (equal (llm-test-normalize '(:foo "aa" :bar "b" :baz "c"))
                 (llm-test-normalize (llm-provider-utils-streaming-accumulate '(:foo "a" :baz "c") '(:foo "a" :bar "b")))))
  (should (equal '(:foo 3) (llm-provider-utils-streaming-accumulate '(:foo 1) '(:foo 2))))
  (should (equal '(:foo "foo bar baz") (llm-provider-utils-streaming-accumulate '(:foo "foo bar") '(:foo " baz")))))

(ert-deftest llm-provider-utils--normalize-args ()
  (should-not (llm-provider-utils--normalize-args :false))
  (should-not (llm-provider-utils--normalize-args :json-false))
  (should (equal '(1 2 nil)
                 (llm-provider-utils--normalize-args '(1 2 :json-false))))
  (should (equal [1 2 nil]
                 (llm-provider-utils--normalize-args [1 2 :json-false])))
  (should (equal '(1 2 [t nil t])
                 (llm-provider-utils--normalize-args '(1 2 [t :false t]))))
  (should (equal '(:a 1 :b nil)
                 (llm-provider-utils--normalize-args '(:a 1 :b :json-false))))
  (should (equal '((a . 1) (b . nil))
                 (llm-provider-utils--normalize-args '((a . 1) (b . :json-false))))))

(cl-defstruct llm-testing-provider (llm-standard-chat-provider) ())

(cl-defmethod llm-provider-populate-tool-uses ((provider llm-testing-provider)
                                               prompt tool-uses))

(ert-deftest llm-provider-utils-execute-tool-uses--missing-tool ()
  (llm-provider-utils-execute-tool-uses
   (make-llm-testing-provider)
   (llm-make-chat-prompt
    ""
    :tools (list
            (llm-make-tool
             :name "tool-a"
             :description "Tool A"
             :function (lambda (&rest args) "Result A")
             :args '())))
   (list
    (make-llm-provider-utils-tool-use
     :id "1"
     :name "tool-b"
     :args '()))
   nil
   nil
   (lambda (results) (ert-fail "Should not succeed."))
   (lambda (type msg)
     (should (equal type 'llm-tool-unknown-tool))
     (should (stringp msg)))))

(ert-deftest llm-provider-utils-execute-tool-uses--unknown-arg ()
  (llm-provider-utils-execute-tool-uses
   (make-llm-testing-provider)
   (llm-make-chat-prompt
    ""
    :tools (list
            (llm-make-tool
             :name "tool-a"
             :description "Tool A"
             :function (lambda (&rest args) "Result A")
             :args '((:name "arg1" :type string :description "Argument 1")))))
   (list
    (make-llm-provider-utils-tool-use
     :id "1"
     :name "tool-a"
     :args '((arg1 . "value1")
             (arg2 . "value2"))))
   nil
   nil
   (lambda (results) (ert-fail "Should not succeed."))
   (lambda (type msg)
     (should (equal type 'llm-tool-unknown-argument))
     (should (stringp msg)))))

(ert-deftest llm-provider-utils-execute-tool-uses--missing-arg ()
  (should-error
   (llm-provider-utils-execute-tool-uses
    (make-llm-testing-provider)
    (llm-make-chat-prompt
     ""
     :tools (list
             (llm-make-tool
              :name "tool-a"
              :description "Tool A"
              :function (lambda (&rest args) "Result A")
              :args '((:name "arg1" :type string :description "Argument 1")))))
    (list
     (make-llm-provider-utils-tool-use
      :id "1"
      :name "tool-a"
      :args '()))
    nil
    nil
    (lambda (results) (ert-fail "Should not succeed."))
    (lambda (type msg)
      (should (equal type 'llm-tool-missing-argument))
      (should (stringp msg))))))

(provide 'llm-provider-utils-test)
;;; llm-provider-utils-test.el ends here
