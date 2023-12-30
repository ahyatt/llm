;;; llm-provider-utils.el --- Functions to make building providers easier -*- lexical-binding: t -*-

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
;; This file provides functions to help build providers. It should only be used
;; by modules implementing an LLM provider.

;;; Code:

(require 'llm)
(require 'seq)

(defun llm-provider-utils-get-system-prompt (prompt &optional example-prelude)
  "From PROMPT, turn the context and examples into a string.
EXAMPLE-PRELUDE is a string to prepend to the examples."
  (concat
   (llm-chat-prompt-context prompt)
   (when (llm-chat-prompt-context prompt) "\n")
   (when (llm-chat-prompt-examples prompt) (or example-prelude
                                               (concat
                                                (if (= (length (llm-chat-prompt-examples prompt)) 1)
                                                    "Here is an example"
                                                  (format "Here are %d examples"
                                                          (length (llm-chat-prompt-examples prompt))))
                                                " of how to respond:\n")))
   (when (llm-chat-prompt-examples prompt) "\n")
   (mapconcat (lambda (example)
                (format "User: %s\nAssistant: %s"
                        (car example)
                        (cdr example)))
              (llm-chat-prompt-examples prompt) "\n")))

(defun llm-provider-utils-combine-to-system-prompt (prompt &optional example-prelude)
  "Add context and examples to a system prompt in PROMPT.
This should be used for providers that have a notion of a system prompt.
If there is a system prompt, and no assistant response, add to it.
If there is no system prompt, create one.
If there is an assistance response, do nothing."
  (unless (seq-some
           (lambda (interaction)
             (eq (llm-chat-prompt-interaction-role interaction) 'assistant))
           (llm-chat-prompt-interactions prompt))
    (let ((system-prompt (seq-find
                          (lambda (interaction)
                            (eq (llm-chat-prompt-interaction-role interaction) 'system))
                          (llm-chat-prompt-interactions prompt)))
          (system-content (llm-provider-utils-get-system-prompt prompt example-prelude)))
      (if system-prompt
          (setf (llm-chat-prompt-interaction-content system-prompt)
                (concat (llm-chat-prompt-interaction-content system-prompt)
                        "\n"
                        system-content))
        (push (make-llm-chat-prompt-interaction
               :role 'system
               :content system-content)
              (llm-chat-prompt-interactions prompt))))))

(defun llm-provider-utils-combine-to-user-prompt (prompt &optional example-prelude)
  "Add context and examples to a user prompt in PROMPT.
This should be used for providers that do not have a notion of a system prompt."
  (when (= (length (llm-chat-prompt-interactions prompt)) 1)
    (when-let ((system-content (llm-provider-utils-get-system-prompt prompt example-prelude)))
      (setf (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt)))
            (concat system-content
                    "\n"
                    (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt))))))))

(defun llm-provider-utils-collapse-history (prompt &optional history-prelude)
  "Collapse history to a single prompt.
This is useful for providers that cannot handle conversations.
Essentially it's a way to fake conversation. Caution: tokens will
eventually run out, though, so this isn't a sustainable way to do
things.  Providers should probably issue a warning when using this."
  (when (> (length (llm-chat-prompt-interactions prompt)) 1)
    (setf (llm-chat-prompt-interactions prompt)
          (list (make-llm-chat-prompt-interaction
                 :role 'user
                 :content 
                 (concat (or history-prelude "Previous interactions:") "\n\n"
                         (mapconcat (lambda (interaction)
                                      (format "%s: %s" (pcase (llm-chat-prompt-interaction-role interaction)
                                                         ('user "User")
                                                         ('assistant "Assistant"))
                                              (llm-chat-prompt-interaction-content interaction)))
                                    (butlast (llm-chat-prompt-interactions prompt)) "\n")
                         "\n\nThe current conversation follows:\n\n"
                         (llm-chat-prompt-interaction-content (car (last (llm-chat-prompt-interactions prompt))))))))))


(provide 'llm-provider-utils)
;;; llm-provider-utils.el ends here
