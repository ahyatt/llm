;;; llm-tester.el --- Helpers for testing LLM implementation

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

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
;; This file contains functions to help test the LLM implementation. Because of
;; LLM's inherent randomness, it isn't possible to have normal unit tests.
;; Humans should be looking at these results and seeing if they make sense.
;; However, we can test a few things automatically, including making sure that
;; no errors are thrown normally, or errors are thrown correctly when they
;; should be.
;;
;; The normal way to use this is to create a provider for each LLM you have
;; access to, and run `llm-tester-all' on it. Or, you can test individual parts
;; with their respective functions.'
;;
;; Both normal output and errors are output to the `*Messages*' buffer.

(require 'llm)

(defun llm-tester-embedding (provider)
  "Test that PROVIDER can provide embeddings."
  (condition-case nil
      (let ((embedding (llm-embedding provider "This is a test.")))
        (if embedding
            (if (eq (type-of embedding) 'vector)
                (if (> (length embedding) 0)
                    (message "SUCCESS: Provider %s provided an embedding of length %d.  First 10 values: %S" (type-of provider)
                             (length embedding)
                             (seq-subseq embedding 0 (min 10 (length embedding))))
                  (message "ERROR: Provider %s returned an empty embedding" (type-of provider))))
          (message "ERROR: Provider %s did not return any embedding" (type-of provider))))
    (not-implemented (message "ERROR: Provider %s could not provide embeddings." (type-of provider)))))

(defun llm-tester-chat (provider)
  "Test that PROVIDER can interact with the LLM chat."
  (condition-case nil
      (let ((response (llm-chat-response 
                   provider
                   (make-llm-chat-prompt
                    :interactions (list
                                   (make-llm-chat-prompt-interaction
                                    :role 'user
                                    :content "Tell me a random cool feature of emacs."))
                    :context "You must answer all questions as if you were the butler Jeeves from Jeeves and Wooster.  Start all interactions with the phrase, 'Very good, sir.'"
                    :examples '(("Tell me the capital of France." . "Very good, sir.  The capital of France is Paris, which I expect you to be familiar with, since you were just there last week with your Aunt Agatha.")
                                ("Could you take me to my favorite place?" . "Very good, sir.  I believe you are referring to the Drone's Club, which I will take you to after you put on your evening attire."))
                    :temperature 0.5
                    :max-tokens 100))))
        (if response
            (if (> (length response) 0)
                (message "SUCCESS: Provider %s provided a response %s" (type-of provider) response)
              (message "ERROR: Provider %s returned an empty response" (type-of provider)))
          (message "ERROR: Provider %s did not return any response" (type-of provider))))
    (not-implemented (message "ERROR: Provider %s could not get a chat." (type-of provider)))))

(defun llm-tester-all (provider)
  "Test all llm functionality for PROVIDER."
  (llm-tester-embedding provider)
  (llm-tester-chat provider))

(provide 'llm-tester)

;;; llm-tester.el ends here
