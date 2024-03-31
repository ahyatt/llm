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

(cl-defstruct (llm-openai (:include llm-standard-full-provider))
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

(cl-defmethod llm-nonfree-message-info ((_ llm-openai))
  "https://openai.com/policies/terms-of-use")

(cl-defmethod llm-provider-embedding-request ((provider llm-openai) string)
  "Return the request to the server for the embedding of STRING.
MODEL is the embedding model to use, or nil to use the default.."
  `(("input" . ,string)
    ("model" . ,(or (llm-openai-embedding-model provider)
                    "text-embedding-3-small"))))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-openai) response)
  "Return the embedding from the server RESPONSE."
  (assoc-default 'embedding (aref (assoc-default 'data response) 0)))

(cl-defmethod llm-openai--check-key ((provider llm-openai))
  (unless (llm-openai-key provider)
    (error "To call Open AI API, add a key to the `llm-openai' provider.")))

(cl-defmethod llm-openai--check-key ((_ llm-openai-compatible))
  ;; It isn't always the case that a key is needed for Open AI compatible APIs.
  )

(cl-defmethod llm-provider-request-prelude ((provider llm-openai))
  (llm-openai--check-key provider))

;; Obsolete, but we keep them here for backward compatibility.
(cl-defgeneric llm-openai--headers (provider)
  "Return the headers to use for a request from PROVIDER.")

(cl-defmethod llm-openai--headers ((provider llm-openai))
  (when (llm-openai-key provider)
    `(("Authorization" . ,(format "Bearer %s" (llm-openai-key provider))))))

(cl-defmethod llm-provider-headers ((provider llm-openai))
  (llm-openai--headers provider))

;; Obsolete, but we keep them here for backward compatibility.
(cl-defgeneric llm-openai--url (provider command)
  "Return the URL for COMMAND for PROVIDER.")

(cl-defmethod llm-openai--url ((_ llm-openai) command)
  (concat "https://api.openai.com/v1/" command))

(cl-defmethod llm-provider-embedding-url ((provider llm-openai))
  (llm-openai--url provider "embeddings"))

(cl-defmethod llm-provider-chat-url ((provider llm-openai))
  (llm-openai--url provider "chat/completions"))

(cl-defmethod llm-openai--url ((provider llm-openai-compatible) command)
  "Return the URL for COMMAND for PROVIDER."
  (concat (llm-openai-compatible-url provider)
          (unless (string-suffix-p "/" (llm-openai-compatible-url provider))
            "/") command))

(cl-defmethod llm-provider-embedding-error-extractor ((_ llm-openai) err-response)
  (let ((errdata (assoc-default 'error err-response)))
      (when errdata
        (format "Open AI returned error: %s message: %s"
                (cdr (assoc 'type errdata))
                (cdr (assoc 'message errdata))))))

(cl-defmethod llm-provider-chat-extract-error ((provider llm-openai) err-response)
  (llm-provider-embedding-error-extractor provider err-response))

(cl-defmethod llm-provider-chat-request ((provider llm-openai) prompt streaming)
  "From PROMPT, create the chat request data to send.
PROVIDER is the Open AI provider.
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
    (push `("model" . ,(or (llm-openai-chat-model provider)
			   "gpt-3.5-turbo-0613")) request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(/ (llm-chat-prompt-temperature prompt) 2.0)) request-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("max_tokens" . ,(llm-chat-prompt-max-tokens prompt)) request-alist))
    (when (llm-chat-prompt-functions prompt)
      (push `("tools" . ,(mapcar #'llm-provider-utils-openai-function-spec
                                 (llm-chat-prompt-functions prompt)))
            request-alist))
    request-alist))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-openai) response)
  (assoc-default 'content
                 (assoc-default 'message (aref (cdr (assoc 'choices response)) 0))))

(cl-defmethod llm-provider-extract-function-calls ((_ llm-openai) response)
  (mapcar (lambda (call)
            (let ((function (cdr (nth 2 call))))
	      (make-llm-provider-utils-function-call
                 :id (assoc-default 'id call)
                 :name (assoc-default 'name function)
                 :args (json-read-from-string (assoc-default 'arguments function)))))
          (assoc-default 'tool_calls
                         (assoc-default 'message
                                        (aref (assoc-default 'choices response) 0)))))

(cl-defmethod llm-provider-populate-function-calls ((_ llm-openai) prompt calls)
  (llm-provider-utils-append-to-prompt
   prompt
   (mapcar (lambda (call)
             `((id . ,(llm-provider-utils-function-call-id call))
               (function (name . ,(llm-provider-utils-function-call-name call))
                         (arguments . ,(json-encode
                                        (llm-provider-utils-function-call-args call))))))
           calls)))

(defvar-local llm-openai-current-response ""
  "The response so far from the server.")

(defvar-local llm-openai-last-response 0
  "The number of the last streaming response we read.
The responses from OpenAI are not numbered, but we just number
them from 1 to however many are sent.")

(defun llm-openai--get-unparsed-json (response)
  "Return the unparsed JSON from RESPONSE.
The response is a list of all the responses, regardless of
whether they have been parsed before or not."
  (with-temp-buffer
    (insert response)
    (let* ((complete-rx (rx (seq line-start "data: ")))
           (end-pos (save-excursion (goto-char (point-max))
                                    (when (search-backward-regexp
                                           complete-rx
                                           nil t)
                                      (line-end-position)))))
      (when end-pos
        (mapcar (lambda (line) (replace-regexp-in-string "data: " "" line))
                (seq-filter
                 (lambda (line)
                   (and (string-match-p complete-rx line)
                        (not (string-match-p (rx (seq line-start "data: [DONE]"))
                                             line))))
                 (split-string (buffer-substring-no-properties 1 end-pos) "\n")))))))

(cl-defmethod llm-provider-extract-partial-response ((_ llm-openai) response)
  "Return the text in the partial chat response from RESPONSE."
  ;; To begin with, we should still be in the buffer with the actual response.
  (let ((current-response llm-openai-current-response)
        (last-response llm-openai-last-response))
    (let* ((all-lines (llm-openai--get-unparsed-json response))
           (processed-lines
           (mapcar (lambda (json)
                     (assoc-default 'content
                                    (assoc-default
                                     'delta
                                     (aref (assoc-default
                                            'choices
                                            (json-read-from-string json))
                                           0))))
                   (seq-subseq all-lines last-response))))
      (when (stringp (car processed-lines))
        ;; The data is a string - a normal response, which we just
        ;; append to current-response (assuming it's also a string,
        ;; which it should be).
        (setq current-response
              (concat current-response (string-join processed-lines ""))))            
      (setq last-response (length all-lines)))
    (when (>= (length current-response) (length llm-openai-current-response))
      (setq llm-openai-current-response current-response)
      (setq llm-openai-last-response last-response))
    (when (> (length llm-openai-current-response) 0)
      llm-openai-current-response)))

(cl-defmethod llm-provider-extract-streamed-function-calls ((_ llm-openai) response)
  (let* ((pieces (mapcar (lambda (json)
			   (assoc-default 'tool_calls
					  (assoc-default
					   'delta
					   (aref (assoc-default
						  'choices
						  (json-read-from-string json))
						 0))))
			 (llm-openai--get-unparsed-json response)))
         (cvec (make-vector (length (car pieces)) (make-llm-provider-utils-function-call))))
    (cl-loop for piece in pieces do
             (cl-loop for call in (append piece nil) do
                      (let* ((index (assoc-default 'index call))
                             (id (assoc-default 'id call))
                             (function (assoc-default 'function call))
                             (name (assoc-default 'name function))
                             (arguments (assoc-default 'arguments function)))
                        (when id
                          (setf (llm-provider-utils-function-call-id (aref cvec index)) id))
                        (when name
                          (setf (llm-provider-utils-function-call-name (aref cvec index)) name))
                        (setf (llm-provider-utils-function-call-args (aref cvec index))
                              (concat (llm-provider-utils-function-call-args (aref cvec index))
                                      arguments)))))
    (cl-loop for call in (append cvec nil)
             do (setf (llm-provider-utils-function-call-args call)
                      (json-read-from-string (llm-provider-utils-function-call-args call)))
             finally return (when (> (length cvec) 0)
			      (append cvec nil)))))

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
