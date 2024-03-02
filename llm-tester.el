;;; llm-tester.el --- Helpers for testing LLM implementation -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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

(defun llm-tester-log (message &rest args)
  "Log MESSAGE and ARGS to the *llm-tester* buffer."
  (let ((buf (get-buffer-create "*llm-tester*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (apply 'format message args))
      (insert "\n"))
    (display-buffer buf)))

(defun llm-tester-embedding-async (provider)
  "Test that PROVIDER can provide embeddings in an async call."
  (llm-tester-log "Testing provider %s for embeddings" (type-of provider))
  (llm-embedding-async provider "This is a test."
                       (lambda (embedding)
                         (if embedding
                             (if (eq (type-of embedding) 'vector)
                                 (if (> (length embedding) 0)
                                     (llm-tester-log "SUCCESS: Provider %s provided an embedding of length %d.  First 10 values: %S" (type-of provider)
                                              (length embedding)
                                              (seq-subseq embedding 0 (min 10 (length embedding))))
                                   (llm-tester-log "ERROR: Provider %s returned an empty embedding" (type-of provider))))
                           (llm-tester-log "ERROR: Provider %s did not return any embedding" (type-of provider))))
                       (lambda (type message)
                         (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message))))

(defun llm-tester-embedding-sync (provider)
  "Test that PROVIDER can provide embeddings in a sync call."
  (llm-tester-log "Testing provider %s for embeddings" (type-of provider))
  (let ((embedding (llm-embedding provider "This is a test.")))
    (if embedding
        (if (eq (type-of embedding) 'vector)
            (if (> (length embedding) 0)
                (llm-tester-log "SUCCESS: Provider %s provided an embedding of length %d.  First 10 values: %S" (type-of provider)
                         (length embedding)
                         (seq-subseq embedding 0 (min 10 (length embedding))))
              (llm-tester-log "ERROR: Provider %s returned an empty embedding" (type-of provider))))
      (llm-tester-log "ERROR: Provider %s did not return any embedding" (type-of provider)))))

(defun llm-tester-chat-async (provider)
  "Test that PROVIDER can interact with the LLM chat."
  (llm-tester-log "Testing provider %s for chat" (type-of provider))
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
           (llm-tester-log "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
         (if response
             (if (> (length response) 0)
                 (llm-tester-log "SUCCESS: Provider %s provided a response %s" (type-of provider) response)
               (llm-tester-log "ERROR: Provider %s returned an empty response" (type-of provider)))
           (llm-tester-log "ERROR: Provider %s did not return any response" (type-of provider))))
       (lambda (type message)
         (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message)))))

(defun llm-tester-chat-sync (provider)
  "Test that PROVIDER can interact with the LLM chat."
  (llm-tester-log "Testing provider %s for chat" (type-of provider))
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
            (llm-tester-log "SUCCESS: Provider %s provided a response %s" (type-of provider) response)
          (llm-tester-log "ERROR: Provider %s returned an empty response" (type-of provider)))
      (llm-tester-log "ERROR: Provider %s did not return any response" (type-of provider)))))

(defun llm-tester-chat-streaming (provider)
  "Test that PROVIDER can stream back LLM chat responses."
  (llm-tester-log "Testing provider %s for streaming chat" (type-of provider))
  (let ((streamed)
        (counter 0)
        (buf (current-buffer)))
    (llm-chat-streaming
     provider
     (make-llm-chat-prompt
      :interactions (list
                     (make-llm-chat-prompt-interaction
                      :role 'user
                      :content "Write a medium length poem in iambic pentameter about the pleasures of using Emacs.  The poem should make snide references to vi."))
      :temperature 0.5)
     (lambda (text)
       (unless (eq buf (current-buffer))
         (llm-tester-log "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
       (cl-incf counter)
       (setq streamed text))
     (lambda (text)
       (unless (eq buf (current-buffer))
         (llm-tester-log "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
       (llm-tester-log "SUCCESS: Provider %s provided a streamed response in %d parts:\n%s" (type-of provider) counter streamed)
       (when (and (member 'streaming (llm-capabilities provider))
                  (not (string= streamed text)))
           (llm-tester-log "ERROR: Provider %s returned a streamed response that was not equal to the final response.  Streamed text %s" (type-of provider) streamed))
       (when (and (member 'streaming (llm-capabilities provider)) (= 0 counter))
           (llm-tester-log "WARNING: Provider %s returned no partial updates!" (type-of provider))))
     (lambda (type message)
       (unless (eq buf (current-buffer))
         (llm-tester-log "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
       (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message)))))

(defun llm-tester-chat-conversation-sync (provider)
  "Test that PROVIDER can handle a conversation."
  (llm-tester-log "Testing provider %s for conversation" (type-of provider))
  (let ((prompt (llm-make-simple-chat-prompt
                 "I'm currently testing conversational abilities.  Please respond to each message with the ordinal number of your response, so just '1' for the first response, '2' for the second, and so on.  It's important that I can verify that you are working with the full conversation history, so please let me know if you seem to be missing anything."))
        (outputs nil))
    (push (llm-chat provider prompt) outputs)
    (llm-chat-prompt-append-response prompt "This is the second message.")
    (push (llm-chat provider prompt) outputs)
    (llm-chat-prompt-append-response prompt "This is the third message.")
    (push (llm-chat provider prompt) outputs)
    (llm-tester-log "SUCCESS: Provider %s provided a conversation with responses %s" (type-of provider)
             (nreverse outputs))))

(defun llm-tester-chat-conversation-async (provider)
  "Test that PROVIDER can handle a conversation."
  (llm-tester-log "Testing provider %s for conversation" (type-of provider))
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
                                          (llm-tester-log "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
                                        (push response outputs)
                                        (llm-chat-prompt-append-response prompt "This is the third message.")
                                        (llm-chat-async provider prompt
                                                        (lambda (response)
                                                          (push response outputs)
                                                          (llm-tester-log "SUCCESS: Provider %s provided a conversation with responses %s" (type-of provider) (nreverse outputs)))
                                                        (lambda (type message)
                                                          (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message))))
                                      (lambda (type message)
                                        (unless (eq buf (current-buffer))
                                          (llm-tester-log "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
                                        (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message))))
                    (lambda (type message)
                      (unless (eq buf (current-buffer))
                        (llm-tester-log "ERROR: Provider %s returned a response not in the original buffer" (type-of provider)))
                      (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message)))))

(defun llm-tester-chat-conversation-streaming (provider)
  "Test that PROVIDER can handle a conversation."
  (llm-tester-log "Testing provider %s for conversation" (type-of provider))
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
               (llm-tester-log "SUCCESS: Provider %s provided a conversation with responses %s" (type-of provider) (buffer-string))
               (kill-buffer buf))))))))))

(defun llm-tester-create-test-function-prompt ()
  "Create a function to test function calling with."
  (make-llm-chat-prompt
                 :context "The user will describe an emacs lisp function they are looking
for, and you need to provide the most likely function you know
of by calling the `describe_function' function."
                 :interactions (list (make-llm-chat-prompt-interaction
                                      :role 'user
                                      :content "I'm looking for a function that will return the current buffer's file name."))
                 :temperature 0.1
                 :functions
                 (list (make-llm-function-call
                        :function (lambda (f) f)
                        :name "describe_function"
                        :description "Takes an elisp function name and shows the user the functions and their descriptions."
                        :args (list (make-llm-function-arg
                                     :name "function_name"
                                     :description "A function name to describe."
                                     :type 'string
                                     :required t))))))

(defun llm-tester-function-calling-sync (provider)
  "Test that PROVIDER can call functions."
  (let ((prompt (llm-tester-create-test-function-prompt)))
    (llm-tester-log "SUCCESS: Provider %s called a function and got result %s"
             (type-of provider)
             (llm-chat provider prompt))))

(defun llm-tester-function-calling-conversation-sync (provider)
  "Test that PROVIDER can call functions in a conversation."
  (let ((prompt (llm-tester-create-test-function-prompt))
        (responses nil))
    (push (llm-chat provider prompt) responses)
    ;; The expectation (a requirement for Gemini) is we call back into the LLM
    ;; with the results of the previous call to get a text response based on the
    ;; function call results.
    (push (llm-chat provider prompt) responses)
    (llm-chat-prompt-append-response prompt "I'm now looking for a function that will return the directory of a filename")
    (push (llm-chat provider prompt) responses)
    (push (llm-chat provider prompt) responses)
    (llm-tester-log "SUCCESS: Provider %s had a function conversation and got results %s"
             (type-of provider)
             (nreverse responses))))

(defun llm-tester-function-calling-async (provider)
  "Test that PROVIDER can call functions asynchronously."
  (let ((prompt (llm-tester-create-test-function-prompt)))
    (llm-chat-async provider prompt
                    (lambda (result)
                      (llm-tester-log "SUCCESS: Provider %s called a function and got a result of %s"
                               (type-of provider) result))
                    (lambda (type message)
                      (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s"
                               (type-of provider) type message)))))

(defun llm-tester-function-calling-conversation-async (provider)
  "Test that PROVIDER can call functions in a conversation."
  (let* ((prompt (llm-tester-create-test-function-prompt))
         (responses nil)
         (error-callback (lambda (type msg) (llm-tester-log "FAILURE: async function calling conversation for %s, error of type %s received: %s" (type-of provider) type msg)))
         (last-callback (lambda (result)
                          (push result responses)
                          (llm-tester-log "SUCCESS: Provider %s had an async function calling conversation, and got results %s"
                                   (type-of provider)
                                   (nreverse responses))))
         (third-callback (lambda (result) (push result responses)
                           (llm-chat-async provider prompt last-callback error-callback)))
         (second-callback (lambda (result) (push result responses)
                            (llm-chat-prompt-append-response prompt "I'm now looking for a function that will return the directory of a filename.")
                            (llm-chat-async provider prompt third-callback error-callback)))
         (first-callback (lambda (result) (push result responses)
                           (llm-chat-async provider prompt second-callback error-callback))))
    (llm-chat-async provider prompt first-callback error-callback)))

(defun llm-tester-function-calling-streaming (provider)
  "Test that PROVIDER can call functions with the streaming API."
  (let ((partial-counts 0))
    (llm-chat-streaming
     provider
     (llm-tester-create-test-function-prompt)
     (lambda (_)
       (cl-incf partial-counts))
     (lambda (text)
       (llm-tester-log "SUCCESS: Provider %s called a function and got a final result of %s"
                (type-of provider) text)
       (unless (= 0 partial-counts)
         (llm-tester-log "WARNING: Provider %s returned partial updates, but it shouldn't for function calling" (type-of provider))))
     (lambda (type message)
       (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s"
                (type-of provider) type message)))))

(defun llm-tester-cancel (provider)
  "Test that PROVIDER can do async calls which can be cancelled."
  (llm-tester-log "Testing provider %s for cancellation" (type-of provider))
  (let ((embedding-request (llm-embedding-async
                            provider "This is a test."
                            (lambda (_)
                              (llm-tester-log "ERROR: Provider %s returned an embedding when it should have been cancelled" (type-of provider)))
                            (lambda (type message)
                              (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message))))
        (chat-async-request (llm-chat-async
                             provider
                             (llm-make-simple-chat-prompt "Please count up to 200.")
                             (lambda (_)
                               (llm-tester-log "ERROR: Provider %s returned a response when it should have been cancelled" (type-of provider)))
                             (lambda (type message)
                               (llm-tester-log "ERROR: Provider %s returned an error of type %s with message %s" (type-of provider) type message)))))
    (llm-cancel-request embedding-request)
    (llm-tester-log "SUCCESS: Provider %s cancelled an async request" (type-of provider))
    (llm-cancel-request chat-async-request)))

(defun llm-tester-all (provider)
  "Test all llm functionality for PROVIDER."
  (let ((separator (string-pad "" 30 ?=)))
    (llm-tester-log "\n%s\nTesting for %s\n%s\n"
                    separator (type-of provider) separator)
    (when (member 'embedding (llm-capabilities provider))
      (llm-tester-embedding-sync provider)
      (llm-tester-embedding-async provider))
    (llm-tester-chat-sync provider)
    (llm-tester-chat-async provider)
    (llm-tester-chat-streaming provider)
    (llm-tester-chat-conversation-sync provider)
    (llm-tester-chat-conversation-async provider)
    (llm-tester-chat-conversation-streaming provider)
    ;; This is too flaky at the moment, subject to race conditions.
    ;; (llm-tester-cancel provider)
    (when (member 'function-calls (llm-capabilities provider))
      (llm-tester-function-calling-sync provider)
      (llm-tester-function-calling-async provider)
      (llm-tester-function-calling-streaming provider)
      (llm-tester-function-calling-conversation-sync provider)
      (llm-tester-function-calling-conversation-async provider))
    (sleep-for 10)
    (llm-tester-log "%s\nEnd of testing for %s\n\n"
                    separator (type-of provider))))

(provide 'llm-tester)

;;; llm-tester.el ends here
