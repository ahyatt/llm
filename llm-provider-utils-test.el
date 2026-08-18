;;; llm-provider-utils-test.el --- Tests for llm-provider-utils -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023-2026  Free Software Foundation, Inc.

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
(require 'llm-test)
(require 'llm)
(require 'seq)

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

(ert-deftest llm-provider-utils-parse-openai-tool-arguments ()
  (should (equal (llm-provider-utils-parse-openai-tool-arguments "")
                 nil))
  (should (equal (llm-provider-utils-parse-openai-tool-arguments
                  "{\"content\":\"├── research_plan.md\"}")
                 '((content . "├── research_plan.md"))))
  (should-error
   (llm-provider-utils-parse-openai-tool-arguments
    "{\"content\":\"├── research_plan.md\"")
   :type 'llm-tool-call-error))

(ert-deftest llm-provider-utils-openai-collect-streaming-tool-uses-invalid-json ()
  (should-error
   (llm-provider-utils-openai-collect-streaming-tool-uses
    [((index . 0)
      (id . "call_1")
      (function
       (name . "write_file")
       (arguments . "{\"content\":\"├── research_plan.md\"")))])
   :type 'llm-tool-call-error))

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

(cl-defmethod llm-provider-append-to-prompt ((provider llm-testing-provider)
                                             prompt content
                                             &optional tool-results)
  (llm-provider-utils-append-to-prompt prompt content tool-results
                                       (if tool-results
                                           'user
                                         'assistant)))

(ert-deftest llm-provider-utils-execute-tool-uses ()
  (let* ((tool (llm-make-tool
                :name "tool-a"
                :description "Tool A"
                :function (lambda (&rest r) r)
                :args '((:name "arg1" :type string :description "Argument 1" :optional nil)
                        (:name "arg2" :type string :description "Argument 2" :optional t))))
         (no-args-tool (llm-make-tool
                        :name "no-args"
                        :description "No Args"
                        :function (lambda () 'success)
                        :args '()))
         (tests `((:name "Successful call no optional"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a" :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))))
                  (:name "Successful call with optional"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a" :args ((arg1 . "value1") (arg2 . "value2"))
                                            :expected-result ("value1" "value2"))))
                  (:name "Successful call with values reversed"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a" :args ((arg2 . "value2") (arg1 . "value1"))
                                            :expected-result ("value1" "value2"))))
                  (:name "Parallel same tool"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a"
                                            :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))
                                     (:name "tool-a"
                                            :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))))
                  (:name "Successful no args tool"
                         :tools ,(list no-args-tool)
                         :tool-uses ((:name "no-args"
                                            :expected-result success)))
                  (:name "Unknown tool"
                         :tools ,(list tool)
                         :tool-uses ((:name "missing-tool" :expected-error (llm-tool-unknown-tool . (:tool "missing-tool")))))
                  (:name "Unknown tool with args"
                         :tools ,(list tool)
                         :tool-uses ((:name "missing-tool" :args ((arg1 . "value1"))
                                            :expected-error (llm-tool-unknown-tool . (:tool "missing-tool")))))
                  (:name "Unknown tool partial"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a"
                                            :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))
                                     (:name "missing-tool" :expected-error (llm-tool-unknown-tool . (:tool "missing-tool")))))
                  (:name "Unknown tool partial reversed"
                         :tools ,(list tool)
                         :tool-uses ((:name "missing-tool" :expected-error (llm-tool-unknown-tool . (:tool "missing-tool")))
                                     (:name "tool-a"
                                            :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))))
                  (:name "Unknown arg"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a"
                                            :args ((bad-arg . "value1"))
                                            :expected-error (llm-tool-unknown-argument . (:tool "tool-a"
                                                                                                :arg "bad-arg")))))
                  (:name "Unknown arg partial result"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a"
                                            :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))
                                     (:name "tool-a"
                                            :args ((bad-arg . "value1"))
                                            :expected-error (llm-tool-unknown-argument . (:tool "tool-a"
                                                                                                :arg "bad-arg")))))
                  (:name "Unknown arg partial result reversed"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a"
                                            :args ((bad-arg . "value1"))
                                            :expected-error (llm-tool-unknown-argument . (:tool "tool-a"
                                                                                                :arg "bad-arg")))
                                     (:name "tool-a"
                                            :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))))
                  (:name "Missing required arg"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a" :args ((arg2 . "value2"))
                                            :expected-error (llm-tool-missing-argument . (:tool "tool-a"
                                                                                                :arg (:name "arg1" :type string :description "Argument 1" :optional nil))))))
                  (:name "Missing required arg partial success"
                         :tools ,(list tool)
                         :tool-uses ((:name "tool-a"
                                            :args ((arg1 . "value1"))
                                            :expected-result ("value1" nil))
                                     (:name "tool-a" :args ((arg2 . "value2"))
                                            :expected-error
                                            (llm-tool-missing-argument
                                             .
                                             (:tool "tool-a"
                                                    :arg (:name "arg1" :type string
                                                                :description "Argument 1" :optional nil)))))))))
    (dolist (test tests)
      (dolist (multi-output (list nil t))
        (dolist (async (list nil t))
          (let ((prompt (llm-make-chat-prompt
                         ""
                         :tools (let ((tools (seq-copy (plist-get test :tools))))
                                  (if async
                                      (mapcar (lambda (tool)
                                                (let ((new-tool (copy-llm-tool tool)))
                                                  (setf (llm-tool-async new-tool) t)
                                                  (setf (llm-tool-function new-tool)
                                                        (lambda (callback &rest r)
                                                          (funcall callback
                                                                   (apply (llm-tool-function tool) r))))
                                                  new-tool))
                                              tools)
                                    tools))))
                callback-executed
                (tool-id 0)
                id-to-tool-use
                (expected-errors (seq-filter #'identity
                                             (mapcan (lambda (call)
                                                       (list (plist-get call :expected-error)))
                                                     (plist-get test :tool-uses)))))
            (ert-info ((format "Test %s, multi-output: %s, async: %s"
                               (plist-get test :name) multi-output async))
              (llm-provider-utils-execute-tool-uses
               (make-llm-testing-provider)
               prompt
               (mapcar (lambda (tool-use)
                         (incf tool-id)
                         (let ((id-str (format "%d" tool-id)))
                           (setf (alist-get id-str id-to-tool-use) tool-use)
                           (make-llm-provider-utils-tool-use
                            :id id-str
                            :name (plist-get tool-use :name)
                            :args (plist-get tool-use :args))))
                       (plist-get test :tool-uses))
               multi-output
               ;; partial result
               '(:text "partial result")
               ;; success callback
               (lambda (result)
                 (setq callback-executed t)
                 (if-let* ((tool-uses (mapcar #'cdr id-to-tool-use))
                           (expected-results (mapcan (lambda (tool-use)
                                                       (when-let* ((expected-result (plist-get tool-use :expected-result)))
                                                         (list (cons (plist-get tool-use :name)
                                                                     expected-result))))
                                                     tool-uses))
                           (full-expectation
                            (if multi-output (append
                                              (list :text "partial result"
                                                    :tool-results expected-results)
                                              (when expected-errors
                                                (list :errors expected-errors)))
                              expected-results)))
                     (ert-info ((format "Testing to see if the result is equal to expected %s" full-expectation))
                       (should (equal result full-expectation)))
                   (ert-fail "success callback should not be called")))
               ;; error callback
               (lambda (type _)
                 (setq callback-executed t)
                 (if expected-errors
                     ;; We use caar here because we just need the type of the *first* expected error.
                     (ert-info ((format "Testing to see if errors are equal to expected errors: %S" (caar expected-errors)))
                       (should (equal (caar expected-errors) type)))
                   (ert-fail "error callback should not be called"))))
              (ert-info ((format "Testing to make sure a callback was called"))
                (should callback-executed))
              (let* ((last-interaction (car (last (llm-chat-prompt-interactions prompt))))
                     (tool-results (llm-chat-prompt-interaction-tool-results last-interaction)))
                (dolist (id-and-tool-use id-to-tool-use)
                  (let* ((tool-result (seq-find (lambda (tool-use)
                                                  (equal (llm-chat-prompt-tool-result-call-id tool-use)
                                                         (car id-and-tool-use)))
                                                tool-results))
                         (expected-result (plist-get (cdr id-and-tool-use) :expected-result)))
                    (ert-info ((format "Tool use %s should populate results in prompt, even if unsuccessful"
                                       id-and-tool-use))
                      (should tool-result)
                      (should (equal (llm-chat-prompt-tool-result-tool-name tool-result)
                                     (plist-get (cdr id-and-tool-use) :name)))
                      (should (or (not expected-result)
                                  (equal (llm-chat-prompt-tool-result-result tool-result)
                                         expected-result))))))))))))))

(provide 'llm-provider-utils-test)
;;; llm-provider-utils-test.el ends here
