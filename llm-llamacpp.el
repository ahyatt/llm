;;; llm-llamacpp.el --- llm module for integrating with llama.cpp. -*- lexical-binding: t -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm
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
;; This file implements the llm functionality defined in llm.el, for llama.cpp,
;; which can be found at https://github.com/ggerganov/llama.cpp.

;;; Code:

(require 'llm)
(require 'cl-lib)
(require 'llm-request)
(require 'llm-provider-utils)
(require 'json)

(defgroup llm-llamacpp nil
  "LLM implementation for llama.cpp."
  :group 'llm)

(defcustom llm-llamacpp-example-prelude "Example of how you should response follow."
  "The prelude to use for examples."
  :type 'string
  :group 'llm-llamacpp)

(defcustom llm-llamacpp-history-prelude "You are in the middle of a conversation between you and a user.  First, we will give you the previous conversation between you ('assistant') and the user, so you have the context, and then give you the latest message for you to response to.  The previous conversation follows."
  "The prelude to use when there has been more than one interaction already.
This is needed because there is no API support for previous chat conversation."
  :type 'string)

(cl-defstruct llm-llamacpp
  "A struct representing a llama.cpp instance."
  (scheme "http") (host "localhost") (port 8080))

(defun llm-llamacpp-url (provider path)
  "From PROVIDER, return the URL for llama.cpp.
PATH is the path to append to the URL, not prefixed with a slash."
  (let ((scheme (llm-llamacpp-scheme provider))
        (host (llm-llamacpp-host provider))
        (port (llm-llamacpp-port provider)))
    (format "%s://%s:%d/%s" scheme host port path)))

(defun llm-llamacpp-get-embedding-from-response (response)
  "From JSON RESPONSE, return the embedding."
  (let ((embedding (assoc-default 'embedding response)))
    (when (and (= 0 (aref embedding 0)) (= 0 (aref embedding 1)))
      (error "llm-llamacpp: embedding might be all 0s, make sure you are starting the server with the --embedding flag"))
    embedding))

(cl-defmethod llm-embedding ((provider llm-llamacpp) string)
  (llm-llamacpp-get-embedding-from-response
   (llm-request-sync (llm-llamacpp-url provider "embedding")
                     :data `((content . ,string)))))

(cl-defmethod llm-embedding-async ((provider llm-llamacpp) string vector-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-llamacpp-url provider "embedding")
                       :data `((content . ,string))
                       :on-success (lambda (data)
                                   (llm-request-callback-in-buffer
                                    buf vector-callback (llm-llamacpp-get-embedding-from-response data)))
                       :on-error (lambda (_ _)
                                   (llm-request-callback-in-buffer
                                    buf error-callback 'error "Unknown error calling llm-llamacpp")))))

(defun llm-llamacpp--prompt-to-text (prompt)
  "From PROMPT, return the text to send to llama.cpp."
  (llm-provider-utils-combine-to-user-prompt prompt llm-llamacpp-example-prelude)
  (llm-provider-utils-collapse-history prompt llm-llamacpp-history-prelude)
  (llm-chat-prompt-interaction-content (car (last (llm-chat-prompt-interactions prompt)))))

(defun llm-llamacpp--chat-request (prompt)
  "From PROMPT, create the chat request data to send."
  (append
   `((prompt . ,(llm-llamacpp--prompt-to-text prompt)))
   (when (llm-chat-prompt-max-tokens prompt)
     `((max_tokens . ,(llm-chat-prompt-max-tokens prompt))))
   (when (llm-chat-prompt-temperature prompt)
     `((temperature . ,(llm-chat-prompt-temperature prompt))))))

(cl-defmethod llm-chat ((provider llm-llamacpp) prompt)
  (let ((output (assoc-default
                 'content
                 (llm-request-sync (llm-llamacpp-url provider "completion")
                                   :data (llm-llamacpp--chat-request prompt)))))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction
                         :role 'assistant
                         :content output))))
    output))

(cl-defmethod llm-chat-async ((provider llm-llamacpp) prompt response-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-llamacpp-url provider "completion")
                       :data (llm-llamacpp--chat-request prompt)
                       :on-success (lambda (data)
                                     (let ((response (assoc-default 'content data)))
                                       (setf (llm-chat-prompt-interactions prompt)
                                             (append (llm-chat-prompt-interactions prompt)
                                                     (list (make-llm-chat-prompt-interaction
                                                            :role 'assistant
                                                            :content response))))
                                       (llm-request-callback-in-buffer
                                        buf response-callback response)))
                       :on-error (lambda (_ _)
                                   (llm-request-callback-in-buffer
                                    buf error-callback 'error "Unknown error calling llm-llamacpp")))))

(defvar-local llm-llamacpp-current-response ""
  "The response so far from the server.")

(defvar-local llm-llamacpp-last-response 0
  "The number of the last streaming response we read.
The responses from OpenAI are not numbered, but we just number
them from 1 to however many are sent.")

(defun llm-llamacpp--get-partial-chat-response (response)
  "From raw streaming output RESPONSE, return the partial chat response."
  (let ((current-response llm-llamacpp-current-response)
        (last-response llm-llamacpp-last-response))
    (with-temp-buffer
      (insert response)
      (let* ((end-of-chunk-rx (rx (seq "\"stop\":" (0+ space) "false}")))
             (end-pos (save-excursion (goto-char (point-max))
                                      (when (search-backward-regexp
                                             end-of-chunk-rx
                                             nil t)
                                        (line-end-position)))))
        (when end-pos
          (let ((all-lines (seq-filter
                            (lambda (line) (string-match-p end-of-chunk-rx line))
                            (split-string (buffer-substring-no-properties 1 end-pos) "\n"))))
            (setq current-response
                  (concat current-response
                          (mapconcat (lambda (line)
                                       (assoc-default 'content
                                                      (json-read-from-string
                                                       (replace-regexp-in-string "data: " "" line))))
                                     (seq-subseq all-lines last-response) "")))
            (setq last-response (length all-lines))))))
    (when (> (length current-response) (length llm-llamacpp-current-response))
        (setq llm-llamacpp-current-response current-response)
        (setq llm-llamacpp-last-response last-response))
    current-response))

(cl-defmethod llm-chat-streaming ((provider llm-llamacpp) prompt partial-callback response-callback error-callback)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-llamacpp-url provider "completion")
                       :data (append (llm-llamacpp--chat-request prompt) '((stream . t)))
                       :on-success-raw (lambda (data)
                                     (let ((response (llm-llamacpp--get-partial-chat-response data)))
                                       (setf (llm-chat-prompt-interactions prompt)
                                             (append (llm-chat-prompt-interactions prompt)
                                                     (list (make-llm-chat-prompt-interaction
                                                            :role 'assistant
                                                            :content response))))
                                       (llm-request-callback-in-buffer
                                        buf response-callback response)))
                       :on-partial (lambda (data)
                                     (when-let ((response (llm-llamacpp--get-partial-chat-response data)))
                                       (llm-request-callback-in-buffer
                                        buf partial-callback response)))
                       :on-error (lambda (_ _)
                                   (llm-request-callback-in-buffer
                                    buf error-callback 'error "Unknown error calling llm-llamacpp")))))

(cl-defmethod llm-name ((_ llm-llamacpp))
  ;; We don't actually know the name of the model, so we have to just name Llama
  ;; CPP itself.
  "Llama CPP")

(provide 'llm-llamacpp)
;;; llm-llamacpp.el ends here
