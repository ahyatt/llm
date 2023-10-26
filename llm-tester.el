;;; llm-tester.el --- Helpers for testing LLM implementation -*- lexical-binding: t -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

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

(defun llm-tester-embedding-async (provider)
  "Test that PROVIDER can provide embeddings in an async call."
  (message "Testing provider %s for embeddings" (type-of provider))
  (llm-embedding-async provider "This is a test."
                       (lambda (embedding)
                         (if embedding
                             (if (eq (type-of embedding) 'vector)
                                 (if (> (length embedding) 0)
                                     (message "SUCCESS: Provider %s provided an embedding of length %d.  First 10 values: %S" (type-of provider)
                                              (length embedding)
                                              (seq-subseq embedding 0 (min 10 (length embedding))))
                                   (message "ERROR: Provider %s returned an empty embedding" (type-of provider))))
                           (message "ERROR: Provider %s did not return any embedding" (type-of provider))))
                       (lambda (type message)
                         (message "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message))))

(defun llm-tester-embedding-sync (provider)
  "Test that PROVIDER can provide embeddings in a sync call."
  (message "Testing provider %s for embeddings" (type-of provider))
  (let ((embedding (llm-embedding provider "This is a test.")))
    (if embedding
        (if (eq (type-of embedding) 'vector)
            (if (> (length embedding) 0)
                (message "SUCCESS: Provider %s provided an embedding of length %d.  First 10 values: %S" (type-of provider)
                         (length embedding)
                         (seq-subseq embedding 0 (min 10 (length embedding))))
              (message "ERROR: Provider %s returned an empty embedding" (type-of provider))))
      (message "ERROR: Provider %s did not return any embedding" (type-of provider)))))

(defun llm-tester-chat-async (provider)
  "Test that PROVIDER can interact with the LLM chat."
  (message "Testing provider %s for chat" (type-of provider))
  (let ((buf (current-buffer)))
    (llm-chat-async
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
        :max-tokens 100)
       (lambda (response)
         (unless (eq buf (current-buffer))
           (message "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
         (if response
             (if (> (length response) 0)
                 (message "SUCCESS: Provider %s provided a response %s" (type-of provider) response)
               (message "ERROR: Provider %s returned an empty response" (type-of provider)))
           (message "ERROR: Provider %s did not return any response" (type-of provider))))
       (lambda (type message)
         (message "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message)))))

(defun llm-tester-chat-sync (provider)
  "Test that PROVIDER can interact with the LLM chat."
  (message "Testing provider %s for chat" (type-of provider))
  (let ((response (llm-chat
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
      (message "ERROR: Provider %s did not return any response" (type-of provider)))))

(defun llm-tester-chat-streaming (provider)
  "Test that PROVIDER can stream back LLM chat responses."
  (message "Testing provider %s for streaming chat" (type-of provider))
  (let ((streamed)
        (counter 0)
        (buf (current-buffer)))
    (llm-chat-streaming
     provider
     (make-llm-chat-prompt
      :interactions (list
                     (make-llm-chat-prompt-interaction
                      :role 'user
                      :content "Write a short poem in iambic pentameter about the pleasures of using Emacs.  The poem should make snide references to vi."))
      :temperature 0.5
      :max-tokens 200)
     (lambda (text)
       (unless (eq buf (current-buffer))
         (message "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
       (cl-incf counter)
       (setq streamed text))
     (lambda (text)
       (unless (eq buf (current-buffer))
         (message "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
       (message "SUCCESS: Provider %s provided a streamed response %s in %d parts, complete text is: %s" (type-of provider) streamed counter text)
       (if (= 0 counter)
           (message "WARNING: Provider %s returned no partial updates!" (type-of provider))))
     (lambda (type message)
       (unless (eq buf (current-buffer))
         (message "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
       (message "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message)))))

(defun llm-tester-chat-conversation-sync (provider)
  "Test that PROVIDER can handle a conversation."
  (message "Testing provider %s for conversation" (type-of provider))
  (let ((prompt (llm-make-simple-chat-prompt
                 "I'm currently testing conversational abilities.  Please respond to each message with the ordinal number of your response, so just '1' for the first response, '2' for the second, and so on.  It's important that I can verify that you are working with the full conversation history, so please let me know if you seem to be missing anything."))
        (outputs nil))
    (push (llm-chat provider prompt) outputs)
    (llm-chat-prompt-append-response prompt "This is the second message.")
    (push (llm-chat provider prompt) outputs)
    (llm-chat-prompt-append-response prompt "This is the third message.")
    (push (llm-chat provider prompt) outputs)
    (message "SUCCESS: Provider %s provided a conversation with responses %s" (type-of provider)
             (nreverse outputs))))

(defun llm-tester-chat-conversation-async (provider)
  "Test that PROVIDER can handle a conversation."
  (message "Testing provider %s for conversation" (type-of provider))
  (let ((prompt (llm-make-simple-chat-prompt
                 "I'm currently testing conversational abilities.  Please respond to each message with the ordinal number of your response, so just '1' for the first response, '2' for the second, and so on.  It's important that I can verify that you are working with the full conversation history, so please let me know if you seem to be missing anything."))
        (outputs nil)
        (buf (current-buffer)))
    (llm-chat-async provider prompt
                    (lambda (response)
                      (push response outputs)
                      (llm-chat-prompt-append-response prompt "This is the second message.")
                      (llm-chat-async provider prompt
                                      (lambda (response)
                                        (unless (eq buf (current-buffer))
                                          (message "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
                                        (push response outputs)
                                        (llm-chat-prompt-append-response prompt "This is the third message.")
                                        (llm-chat-async provider prompt
                                                        (lambda (response)
                                                          (push response outputs)
                                                          (message "SUCCESS: Provider %s provided a conversation with responses %s" (type-of provider) (nreverse outputs)))
                                                        (lambda (type message)
                                                          (message "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message))))
                                      (lambda (type message)
                                        (unless (eq buf (current-buffer))
                                          (message "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
                                        (message "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message))))
                    (lambda (type message)
                      (unless (eq buf (current-buffer))
                        (message "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
                      (message "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message)))))

(defun llm-tester-chat-conversation-streaming (provider)
  "Test that PROVIDER can handle a conversation."
  (message "Testing provider %s for conversation" (type-of provider))
  (let ((prompt (llm-make-simple-chat-prompt
                 "I'm currently testing conversational abilities.  Please respond to each message with the ordinal number of your response, so just '1' for the first response, '2' for the second, and so on.  It's important that I can verify that you are working with the full conversation history, so please let me know if you seem to be missing anything.")))
    (let ((buf (get-buffer-create "*llm-streaming-conversation-tester*")))
      (llm-chat-streaming-to-point
       provider prompt buf (with-current-buffer buf (point-max))
       (lambda ()
         (goto-char (point-max)) (insert "\n")
         (llm-chat-prompt-append-response prompt "This is the second message.")
         (llm-chat-streaming-to-point
          provider prompt
          buf (with-current-buffer buf (point-max))
          (lambda ()
            (goto-char (point-max)) (insert "\n")
            (llm-chat-prompt-append-response prompt "This is the third message.")
            (llm-chat-streaming-to-point
             provider prompt buf (with-current-buffer buf (point-max))
             (lambda ()
               (message "SUCCESS: Provider %s provided a conversation with responses %s" (type-of provider) (buffer-string))
               (kill-buffer buf))))))))))

(defun llm-tester-all (provider)
  "Test all llm functionality for PROVIDER."
  (llm-tester-embedding-sync provider)
  (llm-tester-chat-sync provider)
  (llm-tester-embedding-async provider)
  (llm-tester-chat-async provider)
  (llm-tester-chat-streaming provider)
  (llm-tester-chat-conversation-sync provider)
  (llm-tester-chat-conversation-async provider)
  (llm-tester-chat-conversation-streaming provider))

(provide 'llm-tester)

;;; llm-tester.el ends here
