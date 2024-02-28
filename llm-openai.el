;;; llm-openai.el --- llm module for integrating with Open AI -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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

(defun llm-openai--chat-request (model prompt &optional streaming)
  "From PROMPT, create the chat request data to send.
MODEL is the model name to use.
FUNCTIONS is a list of functions to call, or nil if none.
STREAMING if non-nil, turn on response streaming."
  (let (request-alist)
    (llm-provider-utils-combine-to-system-prompt prompt llm-openai-example-prelude)
    (when streaming (push `("stream" . ,t) request-alist))
    (push `("messages" .
            ,(mapcar (lambda (p)
                       (append
                        `(("role" . ,(llm-chat-prompt-interaction-role p))
                          ("content" . ,(let ((content
                                               (llm-chat-prompt-interaction-content p)))
                                          (if (stringp content) content
                                            (json-encode content)))))
                        (when-let ((fc (llm-chat-prompt-interaction-function-call-result p)))
                          (append
                           (when (llm-chat-prompt-function-call-result-call-id fc)
                             `(("tool_call_id" .
                                ,(llm-chat-prompt-function-call-result-call-id fc))))
                           `(("name" . ,(llm-chat-prompt-function-call-result-function-name fc)))))))
                     (llm-chat-prompt-interactions prompt)))
          request-alist)
    (push `("model" . ,(or model "gpt-3.5-turbo-0613")) request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(/ (llm-chat-prompt-temperature prompt) 2.0)) request-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("max_tokens" . ,(llm-chat-prompt-max-tokens prompt)) request-alist))
    (when (llm-chat-prompt-functions prompt)
      (push `("tools" . ,(mapcar #'llm-provider-utils-openai-function-spec
                                 (llm-chat-prompt-functions prompt)))
            request-alist))
    request-alist))

(defun llm-openai--extract-chat-response (response)
  "Return chat response from server RESPONSE."
  (let ((result (cdr (assoc 'content
                            (cdr (assoc
                                  'message
                                  (aref (cdr (assoc 'choices response)) 0))))))
        (func-result (assoc-default
                      'tool_calls
                      (assoc-default 'message
                                     (aref (assoc-default 'choices response) 0)))))
    (or func-result result)))

(cl-defmethod llm-provider-utils-populate-function-calls ((_ llm-openai) prompt calls)
  (llm-provider-utils-append-to-prompt
   prompt
   (mapcar (lambda (call)
             `((id . ,(llm-provider-utils-function-call-id call))
               (function (name . ,(llm-provider-utils-function-call-name call))
                         (arguments . ,(json-encode
                                        (llm-provider-utils-function-call-args call))))))
           calls)))

(defun llm-openai--normalize-function-calls (response)
  "Transform RESPONSE from what Open AI returns to our neutral format."
  (if (vectorp response)
      (mapcar (lambda (call)
                (let ((function (cl-third call)))
                  (make-llm-provider-utils-function-call
                   :id (assoc-default 'id call)
                   :name (assoc-default 'name function)
                   :args (json-read-from-string (assoc-default 'arguments function)))))
              response)
    response))

(defun llm-openai--process-and-return (provider prompt response &optional error-callback)
  "Process RESPONSE from the PROVIDER.

This function adds the response to the prompt, executes any
functions, and returns the value that the client should get back.

PROMPT is the prompt that needs to be updated with the response."
  (if (and (consp response) (cdr (assoc 'error response)))
      (progn
        (when error-callback
          (funcall error-callback 'error (llm-openai--error-message response)))
        response)
    ;; When it isn't an error
    (llm-provider-utils-process-result
     provider prompt
     (llm-openai--normalize-function-calls
      (if (consp response) (llm-openai--extract-chat-response response)
        (llm-openai--get-partial-chat-response response))))))

(cl-defmethod llm-chat-async ((provider llm-openai) prompt response-callback error-callback)
  (llm-openai--check-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-openai--url provider "chat/completions")
      :headers (llm-openai--headers provider)
      :data (llm-openai--chat-request (llm-openai-chat-model provider) prompt)
      :on-success (lambda (data)
                    (llm-request-callback-in-buffer
                       buf response-callback
                       (llm-openai--process-and-return
                        provider prompt data error-callback)))
      :on-error (lambda (_ data)
                  (let ((errdata (cdr (assoc 'error data))))
                    (llm-request-callback-in-buffer buf error-callback 'error
                             (format "Problem calling Open AI: %s message: %s"
                                     (cdr (assoc 'type errdata))
                                     (cdr (assoc 'message errdata)))))))))

(cl-defmethod llm-chat ((provider llm-openai) prompt)
  (llm-openai--check-key provider)
  (llm-openai--process-and-return
   provider prompt
   (llm-request-sync
    (llm-openai--url provider "chat/completions")
    :headers (llm-openai--headers provider)
    :data (llm-openai--chat-request (llm-openai-chat-model provider)
                                    prompt))))

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
          (let* ((all-lines (seq-filter
                             (lambda (line) (string-match-p complete-rx line))
                             (split-string (buffer-substring-no-properties 1 end-pos) "\n")))
                 (processed-lines
                  (mapcar (lambda (line)
                            (let ((delta (assoc-default
                                          'delta
                                          (aref (assoc-default
                                                 'choices
                                                 (json-read-from-string
                                                  (replace-regexp-in-string "data: " "" line)))
                                                0))))
                              (or (assoc-default 'content delta)
                                  (assoc-default 'tool_calls delta))))
                          (seq-subseq all-lines last-response))))
            (if (stringp (car processed-lines))
                ;; The data is a string - a normal response, which we just
                ;; append to current-response (assuming it's also a string,
                ;; which it should be).
                (setq current-response
                      (concat current-response (string-join processed-lines "")))
              ;; If this is a streaming function call, current-response will be
              ;; a vector of function plists, containing the function name and the arguments
              ;; as JSON.
              (when (equal "" current-response)
                (setq current-response (make-vector (length (car processed-lines))
                                                    nil)))
              (cl-loop for calls in processed-lines do
                       (cl-loop for call in (append calls nil) do
                                (let* ((index (assoc-default 'index call))
                                       (plist (aref current-response index))
                                       (function (assoc-default 'function call))
                                       (name (assoc-default 'name function))
                                       (id (assoc-default 'id call))
                                       (arguments (assoc-default 'arguments function)))
                                  (when name (setq plist (plist-put plist :name name)))
                                  (when id (setq plist (plist-put plist :id id)))
                                  (setq plist (plist-put plist :arguments
                                                         (concat (plist-get plist :arguments)
                                                                 arguments)))
                                  (aset current-response index plist)))))
            
            (setq last-response (length all-lines))))))
    ;; Has to be >= because when we store plists the length doesn't change, but
    ;; we still want to store the new response. For text, it should indeed be
    ;; ever-growing (but sometimes it shrinks and we don't want to store that).
    (when (>= (length current-response) (length llm-openai-current-response))
        (setq llm-openai-current-response current-response)
        (setq llm-openai-last-response last-response))
    ;; If we are dealing with function calling, massage it to look like the
    ;; normal function calling output.
    (if (vectorp current-response)
        (apply #'vector
               (mapcar (lambda (plist)
                         `((id . ,(plist-get plist :id))
                           (type . function)
                           (function
                            .
                            ((name . ,(plist-get plist :name))
                             (arguments . ,(plist-get plist :arguments))))))
                       current-response))
        current-response)))

(cl-defmethod llm-chat-streaming ((provider llm-openai) prompt partial-callback
                                  response-callback error-callback)
  (llm-openai--check-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-openai--url provider "chat/completions")
                       :headers (llm-openai--headers provider)
                       :data (llm-openai--chat-request (llm-openai-chat-model provider) prompt t)
                       :on-error (lambda (_ data)
                                   (let ((errdata (cdr (assoc 'error data))))
                                     (llm-request-callback-in-buffer
                                      buf error-callback 'error
                                      (format "Problem calling Open AI: %s message: %s"
                                              (cdr (assoc 'type errdata))
                                              (cdr (assoc 'message errdata))))))
                       :on-partial (lambda (data)
                                     (when-let ((response (llm-openai--get-partial-chat-response data)))
                                       ;; We only send partial text updates, not
                                       ;; updates related to function calls.
                                       (when (stringp response)
                                         (llm-request-callback-in-buffer buf partial-callback response))))
                       :on-success-raw (lambda (data)
                                         (llm-request-callback-in-buffer
                                          buf
                                          response-callback
                                          (llm-openai--process-and-return
                                           provider prompt
                                           data error-callback))))))

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

(cl-defmethod llm-capabilities ((_ llm-openai))
  (list 'streaming 'embeddings 'function-calls))

(cl-defmethod llm-capabilities ((_ llm-openai-compatible))
  (list 'streaming 'embeddings))

(provide 'llm-openai)

;;; llm-openai.el ends here
