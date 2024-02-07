;;; llm-openai.el --- llm module for integrating with Open AI -*- lexical-binding: t -*-

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
;; This file implements the llm functionality defined in llm.el, for Open AI's
;; API.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-request)
(require 'llm-provider-utils)
(require 'json)

(defgroup llm-openai nil
  "LLM implementation for Open AI."
  :group 'llm)

(defcustom llm-openai-example-prelude "Examples of how you should respond follow."
  "The prelude to use for examples in Open AI chat prompts."
  :type 'string
  :group 'llm-openai)

(cl-defstruct llm-openai
  "A structure for holding information needed by Open AI's API.

KEY is the API key for Open AI, which is required.

CHAT-MODEL is the model to use for chat queries. If unset, it
will use a reasonable default.

EMBEDDING-MODEL is the model to use for embeddings.  If unset, it
will use a reasonable default."
  key chat-model embedding-model)

(cl-defstruct (llm-openai-compatible (:include llm-openai))
  "A structure for other APIs that use the Open AI's API.

URL is the URL to use for the API, up to the command. So, for
example, if the API for chat is at
https://api.example.com/v1/chat, then URL should be
\"https://api.example.com/v1/\"."
  url)

(cl-defmethod llm-nonfree-message-info ((provider llm-openai))
  (ignore provider)
  (cons "Open AI" "https://openai.com/policies/terms-of-use"))

(defun llm-openai--embedding-request (model string)
  "Return the request to the server for the embedding of STRING.
MODEL is the embedding model to use, or nil to use the default.."
  `(("input" . ,string)
    ("model" . ,(or model "text-embedding-3-small"))))

(defun llm-openai--embedding-extract-response (response)
  "Return the embedding from the server RESPONSE."
  (cdr (assoc 'embedding (aref (cdr (assoc 'data response)) 0))))

(defun llm-openai--error-message (err-response)
  "Return a user-visible error message from ERR-RESPONSE."
  (let ((errdata (cdr (assoc 'error err-response))))
    (format "Problem calling Open AI: %s message: %s"
            (cdr (assoc 'type errdata))
            (cdr (assoc 'message errdata)))))

(defun llm-openai--handle-response (response extractor)
  "If RESPONSE is an error, throw it, else call EXTRACTOR."
  (if (cdr (assoc 'error response))
      (error (llm-openai--error-message response))
    (funcall extractor response)))

(cl-defmethod llm-openai--check-key ((provider llm-openai))
  (unless (llm-openai-key provider)
    (error "To call Open AI API, add a key to the `llm-openai' provider.")))

(cl-defmethod llm-openai--check-key ((_ llm-openai-compatible))
  ;; It isn't always the case that a key is needed for Open AI compatible APIs.
  )

(defun llm-openai--headers (provider)
  "From PROVIDER, return the headers to use for a request.
This is just the key, if it exists."
  (when (llm-openai-key provider)
    `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider))))))

(cl-defmethod llm-openai--url ((_ llm-openai) command)
  "Return the URL for COMMAND for PROVIDER."
  (concat "https://api.openai.com/v1/" command))

(cl-defmethod llm-openai--url ((provider llm-openai-compatible) command)
  "Return the URL for COMMAND for PROVIDER."
  (concat (llm-openai-compatible-url provider)
          (unless (string-suffix-p "/" (llm-openai-compatible-url provider))
            "/") command))

(cl-defmethod llm-embedding-async ((provider llm-openai) string vector-callback error-callback)
  (llm-openai--check-key provider)  
  (let ((buf (current-buffer)))
    (llm-request-async (llm-openai--url provider "embeddings")
                       :headers (llm-openai--headers provider)
                       :data (llm-openai--embedding-request (llm-openai-embedding-model provider) string)
                       :on-success (lambda (data)
                                     (llm-request-callback-in-buffer
                                      buf vector-callback (llm-openai--embedding-extract-response data)))
                       :on-error (lambda (_ data) 
                                   (llm-request-callback-in-buffer
                                    buf error-callback 'error
                                    (llm-openai--error-message data))))))

(cl-defmethod llm-embedding ((provider llm-openai) string)
  (llm-openai--check-key provider)
  (llm-openai--handle-response
   (llm-request-sync (llm-openai--url provider "embeddings")
               :headers (llm-openai--headers provider)
               :data (llm-openai--embedding-request (llm-openai-embedding-model provider) string))
   #'llm-openai--embedding-extract-response))

(defun llm-openai--chat-request (model prompt &optional return-json-spec streaming)
  "From PROMPT, create the chat request data to send.
MODEL is the model name to use.
RETURN-JSON-SPEC is the optional specification for the JSON to return.
STREAMING if non-nil, turn on response streaming."
  (let (request-alist)
    (llm-provider-utils-combine-to-system-prompt prompt llm-openai-example-prelude)
    (when streaming (push `("stream" . ,t) request-alist))
    (push `("messages" . ,(mapcar (lambda (p)
                                    `(("role" . ,(pcase (llm-chat-prompt-interaction-role p)
                                                   ('user "user")
                                                   ('system "system")
                                                   ('assistant "assistant")))
                                      ("content" . ,(string-trim (llm-chat-prompt-interaction-content p)))))
                                  (llm-chat-prompt-interactions prompt)))
          request-alist)
    (push `("model" . ,(or model "gpt-3.5-turbo-0613")) request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(/ (llm-chat-prompt-temperature prompt) 2.0)) request-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("max_tokens" . ,(llm-chat-prompt-max-tokens prompt)) request-alist))
    (when return-json-spec
      (push `("functions" . ((("name" . "output")
                              ("parameters" . ,return-json-spec))))
            request-alist)
      (push '("function_call" . (("name" . "output"))) request-alist))
    request-alist))

(defun llm-openai--extract-chat-response (response)
  "Return chat response from server RESPONSE."
  (let ((result (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0))))))
        (func-result (cdr (assoc 'arguments (cdr (assoc 'function_call (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0)))))))))
    (or func-result result)))

(cl-defmethod llm-chat-async ((provider llm-openai) prompt response-callback error-callback)
  (llm-openai--check-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-openai--url provider "chat/completions")
      :headers (llm-openai--headers provider)
      :data (llm-openai--chat-request (llm-openai-chat-model provider) prompt)
      :on-success (lambda (data)
                    (let ((response (llm-openai--extract-chat-response data)))
                      (setf (llm-chat-prompt-interactions prompt)
                            (append (llm-chat-prompt-interactions prompt)
                                    (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
                      (llm-request-callback-in-buffer buf response-callback response)))
      :on-error (lambda (_ data)
                  (let ((errdata (cdr (assoc 'error data))))
                    (llm-request-callback-in-buffer buf error-callback 'error
                             (format "Problem calling Open AI: %s message: %s"
                                     (cdr (assoc 'type errdata))
                                     (cdr (assoc 'message errdata)))))))))

(cl-defmethod llm-chat ((provider llm-openai) prompt)
  (llm-openai--check-key provider)
  (let ((response (llm-openai--handle-response
                   (llm-request-sync (llm-openai--url provider "chat/completions")
                                     :headers (llm-openai--headers provider)
                                     :data (llm-openai--chat-request (llm-openai-chat-model provider)
                                                                     prompt))
                   #'llm-openai--extract-chat-response)))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
    response))

(defvar-local llm-openai-current-response ""
  "The response so far from the server.")

(defvar-local llm-openai-last-response 0
  "The number of the last streaming response we read.
The responses from OpenAI are not numbered, but we just number
them from 1 to however many are sent.")

(defun llm-openai--get-partial-chat-response (response)
  "Return the text in the partial chat response from RESPONSE."
  ;; To begin with, we should still be in the buffer with the actual response.
  (let ((current-response llm-openai-current-response)
        (last-response llm-openai-last-response))
    (with-temp-buffer
      (insert response)
      (let* ((complete-rx (rx (seq "finish_reason\":" (1+ (or ?\[ ?\] alpha)) "}]}" line-end)))
             (end-pos (save-excursion (goto-char (point-max))
                                      (when (search-backward-regexp
                                             complete-rx
                                             nil t)
                                        (line-end-position)))))
        (when end-pos
          (let ((all-lines (seq-filter
                            (lambda (line) (string-match-p complete-rx line))
                            (split-string (buffer-substring-no-properties 1 end-pos) "\n"))))
            (setq current-response
                  (concat current-response
                          (mapconcat (lambda (line)
                                       (assoc-default 'content
                                                      (assoc-default
                                                       'delta
                                                       (aref (assoc-default
                                                              'choices
                                                              (json-read-from-string
                                                               (replace-regexp-in-string "data: " "" line)))
                                                             0))))
                                     (seq-subseq all-lines last-response) "")))
            (setq last-response (length all-lines))))))
    (when (> (length current-response) (length llm-openai-current-response))
        (setq llm-openai-current-response current-response)
        (setq llm-openai-last-response last-response))
    current-response))

(cl-defmethod llm-chat-streaming ((provider llm-openai) prompt partial-callback response-callback error-callback)
  (llm-openai--check-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-openai--url provider "chat/completions")
                       :headers (llm-openai--headers provider)
                       :data (llm-openai--chat-request (llm-openai-chat-model provider) prompt nil t)
                       :on-error (lambda (_ data)
                                   (let ((errdata (cdr (assoc 'error data))))
                                     (llm-request-callback-in-buffer buf error-callback 'error
                                              (format "Problem calling Open AI: %s message: %s"
                                                      (cdr (assoc 'type errdata))
                                                      (cdr (assoc 'message errdata))))))
                       :on-partial (lambda (data)
                                     (when-let ((response (llm-openai--get-partial-chat-response data)))
                                       (llm-request-callback-in-buffer buf partial-callback response)))
                       :on-success-raw (lambda (data)
                                         (let ((response (llm-openai--get-partial-chat-response data)))
                                           (setf (llm-chat-prompt-interactions prompt)
                                                 (append (llm-chat-prompt-interactions prompt)
                                                         (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
                                           (llm-request-callback-in-buffer buf response-callback response))))))

(cl-defmethod llm-name ((_ llm-openai))
  "Open AI")

;; See https://platform.openai.com/docs/models/gpt-4-and-gpt-4-turbo
;; and https://platform.openai.com/docs/models/gpt-3-5.
(cl-defmethod llm-chat-token-limit ((provider llm-openai))
  (let ((model (llm-openai-chat-model provider)))
    (cond
     ((string-match (rx (seq (or ?- ?_) (group-n 1 (+ digit)) ?k)) model)
      (let ((n (string-to-number (match-string 1 model))))
        ;; This looks weird but Open AI really has an extra token for 16k
        ;; models, but not for 32k models.
        (+ (* n 1024) (if (= n 16) 1 0))))
     ((equal model "gpt-4") 8192)
     ((string-match-p (rx (seq "gpt-4-" (+ ascii) "-preview")) model)
       128000)
     ((string-match-p (rx (seq "gpt-4-" (+ digit))) model)
      8192)
     ((string-match-p (rx (seq "gpt-3.5-turbo-1" (+ digit))) model)
      16385)
     ((string-match-p (rx (seq "gpt-3.5-turbo" (opt "-instruct"))) model)
      4096)
     (t 4096))))

(provide 'llm-openai)

;;; llm-openai.el ends here
