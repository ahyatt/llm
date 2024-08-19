;;; llm-provider-utils-test.el --- Tests for llm-provider-utils -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023, 2024  Free Software Foundation, Inc.

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

(require 'llm-provider-utils)

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

(provide 'llm-provider-utils-test)
;;; llm-provider-utils-test.el ends here
