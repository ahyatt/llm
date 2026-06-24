;;; llm-openai.el --- llm module for integrating with Open AI -*- lexical-binding: t; package-lint-main-file: "llm.el"; byte-compile-docstring-max-column: 200-*-

;; Copyright (c) 2023-2026  Free Software Foundation, Inc.

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
(require 'llm-provider-utils)
(require 'llm-models)
(require 'json)
(require 'plz)
(require 'plz-event-source)

(defgroup llm-openai nil
  "LLM implementation for Open AI."
  :group 'llm)

(defcustom llm-openai-example-prelude "Examples of how you should respond follow."
  "The prelude to use for examples in Open AI chat prompts."
  :type 'string
  :group 'llm-openai)

;; This uses the Responses API.
(cl-defstruct (llm-openai
               (:include llm-standard-full-provider)
               (:constructor make-llm-openai
                             (&key default-chat-temperature
                                   default-chat-max-tokens
                                   default-chat-non-standard-params
                                   ((:key raw-key))
                                   (chat-model "gpt-5.4-mini")
                                   (embedding-model "text-embedding-3-small")
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key)))))
  "A structure for holding information needed by Open AI's API.

KEY is the API key for Open AI, which is required.

CHAT-MODEL is the model to use for chat queries.  If unset, it
will use a reasonable default.

EMBEDDING-MODEL is the model to use for embeddings.  If unset, it
will use a reasonable default."
  key (chat-model "gpt-5.4-mini") (embedding-model "text-embedding-3-small"))

(cl-defstruct (llm-openai-compatible
               (:include llm-openai
                         (chat-model "unset")
                         (embedding-model "unset"))
               (:constructor make-llm-openai-compatible
                             (&key default-chat-temperature
                                   default-chat-max-tokens
                                   default-chat-non-standard-params
                                   ((:key raw-key))
                                   (chat-model "unset")
                                   (embedding-model "unset")
                                   url
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key)))))
  "A structure for other APIs that use the Open AI's API.

URL is the URL to use for the API, up to the command.  So, for
example, if the API for chat is at
https://api.example.com/v1/chat, then URL should be
\"https://api.example.com/v1/\"."
  url)

(cl-defstruct (llm-openrouter
               (:include llm-openai-compatible
                         (url "https://openrouter.ai/api/v1/"))
               (:constructor make-llm-openrouter
                             (&key default-chat-temperature
                                   default-chat-max-tokens
                                   default-chat-non-standard-params
                                   ((:key raw-key))
                                   (chat-model "unset")
                                   (embedding-model "unset")
                                   (url "https://openrouter.ai/api/v1/")
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key)))))
  "A structure for Open Router.

This is mostly compatible with Open AI's API but has some minor API
differences.")

(cl-defgeneric llm-openai-primary-chat-model (provider)
  "Return the primary chat model for the Open AI PROVIDER.")

(cl-defmethod llm-openai-primary-chat-model ((provider llm-openai))
  (llm-openai-chat-model provider))

(cl-defmethod llm-openai-primary-chat-model ((provider llm-openrouter))
  (let ((model-data (llm-openai-compatible-chat-model provider)))
    (if (listp model-data)
        (car model-data)
      model-data)))

(cl-defmethod llm-nonfree-message-info ((_ llm-openai))
  "Return Open AI's nonfree terms of service."
  "https://openai.com/policies/terms-of-use")

(cl-defmethod llm-provider-embedding-request ((provider llm-openai) string-or-list)
  "Return the request to the server for the embedding of STRING-OR-LIST.
PROVIDER is the Open AI provider struct."
  `(:input ,(if (listp string-or-list)
                (apply #'vector string-or-list)
              string-or-list)
           :model ,(llm-openai-embedding-model provider)))

(cl-defmethod llm-provider-batch-embeddings-request ((provider llm-openai) batch)
  (llm-provider-embedding-request provider batch))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-openai) response)
  "Return the embedding from the server RESPONSE."
  (assoc-default 'embedding (aref (assoc-default 'data response) 0)))

(cl-defmethod llm-provider-batch-embeddings-extract-result ((_ llm-openai) response)
  "Return the embedding from the server RESPONSE."
  (let* ((data (assoc-default 'data response))
         (vec (make-vector (length data) nil)))
    (mapc (lambda (d)
            (aset vec (assoc-default 'index d)
                  (assoc-default 'embedding d)))
          data)
    (append vec nil)))

(cl-defgeneric llm-openai--check-key (provider)
  "Check that the key is set for the Open AI PROVIDER.")

(cl-defmethod llm-openai--check-key ((provider llm-openai))
  (unless (llm-openai-key provider)
    (signal 'llm-provider-unconfigured
            '("To call Open AI API, add a key to the `llm-openai' provider"))))

(cl-defmethod llm-openai--check-key ((_ llm-openai-compatible))
  ;; It isn't always the case that a key is needed for Open AI compatible APIs.
  )

(cl-defmethod llm-provider-request-prelude ((provider llm-openai))
  (llm-openai--check-key provider))

;; Obsolete, but we keep them here for backward compatibility.
(cl-defgeneric llm-openai--headers (provider)
  "Return the headers to use for a request from PROVIDER.")

(cl-defmethod llm-openai--headers ((provider llm-openai))
  (when-let* ((key (llm-openai-key provider)))
    ;; If the key is a function, call it.  The `auth-source' API uses functions
    ;; to wrap secrets and to obfuscate them in the Emacs heap.
    (when (functionp key)
      (setq key (funcall key)))
    ;; Encode the API key to ensure it is unibyte.  The request library gets
    ;; confused by multibyte headers, which turn the entire body multibyte if
    ;; there’s a non-ascii character, regardless of encoding.  And API keys are
    ;; likely to be obtained from external sources like shell-command-to-string,
    ;; which always returns multibyte.
    `(("Authorization" . ,(format "Bearer %s" (encode-coding-string key 'utf-8))))))

(cl-defmethod llm-provider-headers ((provider llm-openai))
  (llm-openai--headers provider))

;; Obsolete, but we keep them here for backward compatibility.
(cl-defgeneric llm-openai--url (provider command)
  "Return the URL for COMMAND for PROVIDER.")

(cl-defmethod llm-openai--url ((_ llm-openai) command)
  (concat "https://api.openai.com/v1/" command))

(cl-defmethod llm-provider-embedding-url ((provider llm-openai) &optional _)
  (llm-openai--url provider "embeddings"))

(cl-defmethod llm-provider-chat-url ((provider llm-openai))
  (llm-openai--url provider "responses"))

(cl-defmethod llm-provider-chat-url ((provider llm-openai-compatible))
  (llm-openai--url provider "chat/completions"))

(cl-defmethod llm-openai--url ((provider llm-openai-compatible) command)
  "Return the URL for COMMAND for PROVIDER."
  (concat (llm-openai-compatible-url provider)
          (unless (string-suffix-p "/" (llm-openai-compatible-url provider))
            "/") command))

(cl-defmethod llm-provider-embedding-extract-error ((_ llm-openai) err-response)
  (let ((errdata (assoc-default 'error err-response)))
    (when errdata
      (format "Open AI returned error: %s message: %s"
              (cdr (assoc 'type errdata))
              (cdr (assoc 'message errdata))))))

(cl-defmethod llm-provider-chat-extract-error ((provider llm-openai) err-response)
  (llm-provider-embedding-extract-error provider err-response))

(defun llm-openai--response-format (format)
  "Return the Open AI response format for FORMAT."
  (if (eq format 'json) '(:type "json_object")
    ;; If not JSON, this must be a json response spec.
    `(:type "json_schema"
            :json_schema (:name "response"
                                :strict t
                                :schema ,(append
                                          (llm-provider-utils-convert-to-serializable
                                           format)
                                          '(:additionalProperties :false))))))

(defun llm-openai--responses-api-response-format (format)
  "Return the Open AI response format for FORMAT."
  (if (eq format 'json) '(:type "json_object")
    ;; If not JSON, this must be a json response spec.
    `(:type "json_schema"
            :name "response"
            :strict t
            :schema ,(append
                      (llm-provider-utils-convert-to-serializable
                       format)
                      '(:additionalProperties :false)))))

(cl-defgeneric llm-openai--build-model (provider)
  "Get the model setting for the request for PROVIDER.")

(cl-defmethod llm-openai--build-model ((provider llm-openai))
  (list :model (llm-openai-chat-model provider)))

(cl-defmethod llm-openai--build-model ((provider llm-openrouter))
  (let ((model (llm-openai-compatible-chat-model provider)))
    (if (listp model)
        (list :models (apply #'vector model))
      (list :model model))))

(defun llm-openai--build-streaming (streaming)
  "Add streaming field if STREAMING is non-nil."
  (when streaming
    (list :stream t
          :stream_options '(:include_usage t))))

(defun llm-openai--build-temperature (prompt)
  "Build the temperature field if present in PROMPT."
  (when (llm-chat-prompt-temperature prompt)
    (list :temperature (* (llm-chat-prompt-temperature prompt) 2.0))))

(defun llm-openai--build-max-tokens (prompt)
  "Build the max_tokens field if present in PROMPT."
  (when (llm-chat-prompt-max-tokens prompt)
    (list :max_tokens (llm-chat-prompt-max-tokens prompt))))

(defun llm-openai--responses-api-build-response-format (prompt)
  "Build the response_format field if present in PROMPT."
  (when (and
         (llm-chat-prompt-response-format prompt))
    (list :text
          (list :format
                (llm-openai--responses-api-response-format (llm-chat-prompt-response-format prompt))))))

(defun llm-openai--build-response-format (provider prompt)
  "Build the response_format field if present in PROMPT.

PROVIDER is the provider, which, if the model doesn't support this, will
not add any configuration."
  (when (and (member 'json-response (llm-capabilities provider))
             (llm-chat-prompt-response-format prompt))
    (list :response_format
          (llm-openai--response-format (llm-chat-prompt-response-format prompt)))))

(defun llm-openai--build-tools (prompt)
  "Build the tools field if tools are present in PROMPT."
  (when (llm-chat-prompt-tools prompt)
    (list :tools (vconcat (mapcar #'llm-provider-utils-openai-tool-spec
                                  (llm-chat-prompt-tools prompt))))))

(defun llm-openai--responses-api-build-tools (prompt)
  "Build the tools field for the Responses API if tools are present in PROMPT."
  (when (llm-chat-prompt-tools prompt)
    (list :tools (vconcat (mapcar (lambda (tool)
                                    (list :type "function"
                                          :name (llm-tool-name tool)
                                          :description (llm-tool-description tool)
                                          :parameters (llm-provider-utils-openai-arguments
                                                       (llm-tool-args tool))))
                                  (llm-chat-prompt-tools prompt))))))

(defun llm-openai--build-tool-choice (prompt)
  "Build the tool_choice field if present in PROMPT."
  (when-let* ((options (llm-chat-prompt-tool-options prompt)))
    (list :tool_choice
          (pcase (llm-tool-options-tool-choice options)
            ('auto "auto")
            ('none "none")
            ('any "required")
            ((pred stringp)
             (list
              :function (list :name (llm-tool-options-tool-choice options))
              :type "function"))
            (_ (signal 'llm-not-supported
                       (list (format "Unknown tool choice option: %s"
                                     (llm-tool-options-tool-choice options)))))))))

(defun llm-openai--build-tool-interaction (interaction)
  "Build the tool interaction for INTERACTION."
  (mapcar
   (lambda (tool-result)
     (let ((msg-plist
            (list
             :role "tool"
             :name (llm-chat-prompt-tool-result-tool-name tool-result)
             :content (format "Result of tool call is %s" (llm-chat-prompt-tool-result-result tool-result)))))
       (when (llm-chat-prompt-tool-result-call-id tool-result)
         (setq msg-plist
               (plist-put msg-plist :tool_call_id
                          (llm-chat-prompt-tool-result-call-id tool-result))))
       msg-plist))
   (llm-chat-prompt-interaction-tool-results interaction)))

(defun llm-openai--responses-api-build-tool-interaction (interaction)
  "Build the tool interaction for INTERACTION for the Response API."
  (mapcar
   (lambda (tool-result)
     (list :call_id (llm-chat-prompt-tool-result-call-id tool-result)
           :output (format "%s" (llm-chat-prompt-tool-result-result tool-result))
           :type "function_call_output"))
   (llm-chat-prompt-interaction-tool-results interaction)))

(defun llm-openai--build-tool-uses (fcs)
  "Convert back from the generic representation to the Open AI.

FCS is a list of `llm-provider-utils-tool-use' structs."
  (vconcat
   (mapcar (lambda (fc)
             `(:id ,(llm-provider-utils-tool-use-id fc)
                   :type "function"
                   :function
                   (:name ,(llm-provider-utils-tool-use-name fc)
                          :arguments ,(llm-provider-utils-json-serialize
                                       (llm-provider-utils-tool-use-args fc)))))
           fcs)))

(defun llm-openai--responses-api-build-tool-uses (fcs)
  "Convert back from the generic representation to the Open AI.

FCS is a list of `llm-provider-utils-tool-use' structs."
  (mapcar (lambda (fc)
            (list
             :call_id (llm-provider-utils-tool-use-id fc)
             :type "function_call"
             :name (llm-provider-utils-tool-use-name fc)
             :arguments (llm-provider-utils-json-serialize
                         (llm-provider-utils-tool-use-args fc))))
          fcs))

(cl-defgeneric llm-openai--build-reasoning (provider prompt)
  "Build the reasoning field for PROVIDER and PROMPT.")

(defun llm-openai--parse-version (model)
  "Parse the major and minor version from MODEL.

If it cannot be parsed, return nil.  Otherwise, return a cons of (MAJOR
. MINOR)."
  (when (string-match "gpt-\\([0-9]+\\)\\.\\([0-9]+\\)" model)
    (cons (string-to-number (match-string 1 model))
          (string-to-number (match-string 2 model)))))

(defun llm-openai--supports-reasoning (provider)
  "Return non-nil if PROVIDER supports reasoning effort.

The Open AI models have inconsistent and confusing support pre-5.2, so
we need to check for a model post 5.2 (if it supports reasoning at all)."
  (and (member 'reasoning (llm-capabilities provider))
       (let* ((model (llm-openai-primary-chat-model provider))
              (major-minor (llm-openai--parse-version model)))
         (and major-minor
              (or (>= (car major-minor) 6)
                  (and
                   (>= (car major-minor) 5)
                   (>= (cdr major-minor) 2)))))))

(cl-defmethod llm-openai--build-reasoning ((provider llm-openai) prompt)
  (when (llm-openai--supports-reasoning provider)
    (if (llm-chat-prompt-reasoning prompt)
        (list :reasoning
              (list :summary "auto"
                    :effort
                    (pcase (llm-chat-prompt-reasoning prompt)
                      ('none "none")
                      ('light "low")
                      ('medium "medium")
                      ('maximum "xhigh")
                      (_ (signal 'llm-not-supported
                                 (list (format "Unknown reasoning effort option: %s"
                                               (llm-chat-prompt-reasoning prompt))))))))
      '(:reasoning (:summary "auto" :effort "medium")))))

(defun llm-openai--responses-api-build-messages (prompt)
  "Build the :messages field based on interactions in PROMPT."
  (let ((interactions (llm-chat-prompt-interactions prompt)))
    (list
     :input
     (vconcat
      (mapcan
       (lambda (interaction)
         (let ((content (llm-chat-prompt-interaction-content interaction)))
           (cond
            ((llm-chat-prompt-interaction-tool-results interaction)
             (llm-openai--responses-api-build-tool-interaction interaction))
            ((and (consp content)
                  (llm-provider-utils-tool-use-p (car content)))
             (llm-openai--responses-api-build-tool-uses content))
            ;; Handle regular interactions
            (t (list
                (let ((msg-plist
                       (list :role (symbol-name (llm-chat-prompt-interaction-role interaction)))))
                  (setq msg-plist
                        (plist-put msg-plist :content
                                   (cond
                                    ((llm-multipart-p content)
                                     (vconcat
                                      (mapcar
                                       (lambda (part)
                                         (if (llm-media-p part)
                                             (list :type "input_image"
                                                   :image_url
                                                   (concat
                                                    "data:"
                                                    (llm-media-mime-type part)
                                                    ";base64,"
                                                    (base64-encode-string
                                                     (llm-media-data part) t)))
                                           (list :type "input_text" :text part)))
                                       (llm-multipart-parts content))))
                                    (t content))))
                  (when-let* ((multi-turn-plist (llm-chat-prompt-interaction-multi-turn-plist interaction))
                              (reasoning-id (plist-get multi-turn-plist :openai-reasoning-id))
                              (encrypted-reasoning (plist-get multi-turn-plist :openai-encrypted-reasoning)))
                    (setq msg-plist
                          (plist-put msg-plist :reasoning
                                     (list :id reasoning-id
                                           :encrypted_content encrypted-reasoning))))
                  msg-plist))))))
       interactions)))))

(defun llm-openai--build-messages (prompt)
  "Build the :messages field based on interactions in PROMPT."
  (let ((interactions (llm-chat-prompt-interactions prompt)))
    (list
     :messages
     (vconcat
      (mapcan
       (lambda (interaction)
         (if (llm-chat-prompt-interaction-tool-results interaction)
             (llm-openai--build-tool-interaction interaction)
           ;; Handle regular interactions
           (list
            (let ((msg-plist
                   (list :role (symbol-name (llm-chat-prompt-interaction-role interaction)))))
              (when-let* ((content (llm-chat-prompt-interaction-content interaction)))
                (if (and (consp content)
                         (llm-provider-utils-tool-use-p (car content)))
                    (setq msg-plist
                          (plist-put msg-plist :tool_calls
                                     (llm-openai--build-tool-uses content)))
                  (setq msg-plist
                        (plist-put msg-plist :content
                                   (cond
                                    ((llm-multipart-p content)
                                     (vconcat
                                      (mapcar
                                       (lambda (part)
                                         (if (llm-media-p part)
                                             (llm-openai--chat-completions-media-part part)
                                           (list :type "text" :text part)))
                                       (llm-multipart-parts content))))
                                    (t content))))))
              (when-let* ((multi-turn-plist (llm-chat-prompt-interaction-multi-turn-plist interaction))
                          (reasoning-id (plist-get multi-turn-plist :openai-reasoning-id))
                          (encrypted-reasoning (plist-get multi-turn-plist :openai-encrypted-reasoning)))
                (setq msg-plist
                      (plist-put msg-plist :reasoning
                                 (list :id reasoning-id
                                       :encrypted_content encrypted-reasoning))))
              msg-plist))))
       interactions)))))

(defun llm-openai--chat-completions-media-part (media)
  "Convert MEDIA to an OpenAI Chat Completions content part."
  (let ((mime-type (llm-media-mime-type media))
        (data (base64-encode-string (llm-media-data media) t)))
    (cond
     ((string-prefix-p "image/" mime-type)
      (list :type "image_url"
            :image_url
            (list :url (concat "data:" mime-type ";base64," data))))
     ((member mime-type '("audio/wav" "audio/x-wav" "audio/mpeg"))
      (list :type "input_audio"
            :input_audio
            (list :data data
                  :format (if (equal mime-type "audio/mpeg") "mp3" "wav"))))
     (t
      (signal 'llm-not-supported
              (list (format "Unsupported OpenAI Chat Completions media type: %s"
                            mime-type)))))))

(defun llm-provider-merge-non-standard-params (non-standard-params request-plist)
  "Merge NON-STANDARD-PARAMS (alist) into REQUEST-PLIST."
  (dolist (param non-standard-params request-plist)
    (let ((key (car param))
          (val (cdr param)))
      (setq request-plist
            (plist-put request-plist
                       (if (keywordp key) key (intern (concat ":" (if (symbolp key)
                                                                      (symbol-name key)
                                                                    key))))
                       val)))))

(cl-defmethod llm-provider-chat-request ((provider llm-openai) prompt streaming)
  "From PROMPT, create the chat request data to send.
PROVIDER is the Open AI provider.
STREAMING if non-nil, turn on response streaming."
  (let ((non-standard-params (llm-chat-prompt-non-standard-params prompt))
        request-plist)

    ;; Combine all the parts
    (setq request-plist
          (append
           (llm-openai--build-model provider)
           (llm-openai--build-reasoning provider prompt)
           (llm-openai--build-temperature prompt)
           (llm-openai--build-max-tokens prompt)
           (llm-openai--responses-api-build-response-format prompt)
           (llm-openai--responses-api-build-tools prompt)
           (llm-openai--build-tool-choice prompt)
           (llm-openai--responses-api-build-messages prompt)
           (when-let* ((instructions (llm-provider-utils-get-system-prompt prompt)))
             (when (> (length instructions) 0)
               (list :instructions instructions)))
           (list :store :false)
           (list :stream (if streaming t :false))
           '(:include ["reasoning.encrypted_content"])))

    ;; Merge non-standard params
    (setq request-plist (llm-provider-merge-non-standard-params non-standard-params request-plist))

    ;; Return the final request plist
    request-plist))

(cl-defmethod llm-provider-chat-request ((provider llm-openai-compatible) prompt streaming)
  "From PROMPT, create the chat request data to send.
PROVIDER is the Open AI provider.
STREAMING if non-nil, turn on response streaming."
  (llm-provider-utils-combine-to-system-prompt prompt llm-openai-example-prelude)
  (let ((non-standard-params (llm-chat-prompt-non-standard-params prompt))
        request-plist)

    ;; Combine all the parts
    (setq request-plist
          (append
           (llm-openai--build-model provider)
           (llm-openai--build-streaming streaming)
           (llm-openai--build-reasoning provider prompt)
           (llm-openai--build-temperature prompt)
           (llm-openai--build-max-tokens prompt)
           (llm-openai--build-response-format provider prompt)
           (llm-openai--build-tools prompt)
           (llm-openai--build-tool-choice prompt)
           (llm-openai--build-messages prompt)))

    ;; Merge non-standard params
    (setq request-plist (llm-provider-merge-non-standard-params non-standard-params request-plist))

    ;; Return the final request plist
    request-plist))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-openai) response)
  (when-let* ((output (assoc-default 'output response))
              (content (assoc-default 'content (seq-find (lambda (item) (equal (assoc-default 'type item)
                                                                               "message"))
                                                         output)))
              (output-text (assoc-default 'text
                                          (seq-find (lambda (item) (equal (assoc-default 'type item) "output_text"))
                                                    content))))
    output-text))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-openai-compatible) response)
  (assoc-default 'content
                 (assoc-default 'message (aref (cdr (assoc 'choices response)) 0))))

(cl-defmethod llm-provider-extract-token-use ((_ llm-openai) response)
  (let ((usage (assoc-default 'usage response)))
    `(:input-tokens ,(assoc-default 'input_tokens usage)
                    :output-tokens ,(assoc-default 'output_tokens usage))))

(cl-defmethod llm-provider-extract-token-use ((_ llm-openai-compatible) response)
  (let ((usage (assoc-default 'usage response)))
    `(:input-tokens ,(assoc-default 'prompt_tokens usage))))

(cl-defmethod llm-provider-extract-tool-uses ((_ llm-openai) response)
  (mapcar (lambda (call)
            (make-llm-provider-utils-tool-use
             :id (assoc-default 'id call)
             :name (assoc-default 'name call)
             :args (llm-provider-utils-parse-openai-tool-arguments
                    (assoc-default 'arguments call))))
          (seq-filter (lambda (item) (equal (assoc-default 'type item) "function_call"))
                      (assoc-default 'output response))))

(cl-defmethod llm-provider-extract-tool-uses ((_ llm-openai-compatible) response)
  (mapcar (lambda (call)
            (let ((tool (assoc-default 'function call)))
              (make-llm-provider-utils-tool-use
               :id (assoc-default 'id call)
               :name (assoc-default 'name tool)
               :args (llm-provider-utils-parse-openai-tool-arguments
                      (assoc-default 'arguments tool)))))
          (assoc-default 'tool_calls
                         (assoc-default 'message
                                        (aref (assoc-default 'choices response) 0)))))

(cl-defmethod llm-provider-extract-reasoning ((_ llm-openai) response)
  (when-let* ((output (assoc-default 'output response))
              (content (assoc-default 'summary
                                      (seq-find (lambda (item) (equal (assoc-default 'type item) "reasoning"))
                                                output))))
    (mapconcat #'identity
               (cl-loop for item across content
                        when (equal (assoc-default 'type item) "summary_text")
                        collect (assoc-default 'text item))
               " ")))

(cl-defmethod llm-provider-extract-for-multi-turn ((_ llm-openai) response)
  (when-let* ((reasoning (seq-find (lambda (item) (equal (assoc-default 'type item) "reasoning"))
                                   (assoc-default 'output response)))
              (id (assoc-default 'id reasoning))
              (encrypted-reasoning (assoc-default 'encrypted_content reasoning)))
    `(:openai-encrypted-reasoning ,encrypted-reasoning
                                  :openai-reasoning-id ,id)))

;; Several Open AI compatible providers such as oMLX include a
;; "reasoning_content" field in the response.
(cl-defmethod llm-provider-extract-reasoning ((_ llm-openai-compatible) response)
  (when-let* ((message (assoc-default 'message (aref (cdr (assoc 'choices response)) 0))))
    (or
     (assoc-default 'reasoning_content message)
     (assoc-default 'reasoning message))))


(cl-defmethod llm-provider-populate-tool-uses ((_ llm-openai) prompt tool-uses)
  (llm-provider-utils-append-to-prompt prompt tool-uses nil nil 'assistant))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-openai-compatible) receiver _)
  (cons 'text/event-stream
        (plz-event-source:text/event-stream
         :events `((message
                    .
                    ,(lambda (event)
                       (let ((data (plz-event-source-event-data event)))
                         (unless (equal data "[DONE]")
                           (let ((response-alist (json-parse-string data :object-type 'alist)))
                             (when-let* ((choices (assoc-default 'choices response-alist))
										 (delta (and (> (length choices) 0)
													 (assoc-default 'delta (aref choices 0)))))
							   (let ((text (llm-provider-utils-json-val (assoc-default 'content delta)))
									 (tool-calls (llm-provider-utils-json-val (assoc-default 'tool_calls delta)))
									 (reasoning (llm-provider-utils-json-val
												 (or (assoc-default 'reasoning delta)
													 (assoc-default 'reasoning_content delta)))))
								 (when-let* ((output (append
													  (when text `(:text ,text))
													  (when tool-calls `(:tool-uses-raw ,tool-calls))
													  (when reasoning `(:reasoning ,reasoning)))))
								   (funcall receiver output))))
                             (when-let* ((usage (assoc-default 'usage response-alist)))
                               (when (not (eq usage :null))
                                 (funcall receiver
                                          `(:input-tokens ,(assoc-default 'prompt_tokens usage))))))))))))))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-openai) receiver _)
  (cons 'text/event-stream
        (plz-event-source:text/event-stream
         :events `((response.completed
                    .
                    ,(lambda (event)
                       (when-let* ((data (plz-event-source-event-data event))
                                   (response-alist (json-parse-string data :object-type 'alist))
                                   (response (assoc-default 'response response-alist))
                                   (usage (assoc-default 'usage response)))
                         (funcall receiver (list :input-tokens (assoc-default 'input_tokens usage)
                                                 :output-tokens (assoc-default 'output_tokens usage))))))
                   (response.output_text.delta
                    .
                    ,(lambda (event)
                       (let* ((data (plz-event-source-event-data event))
                              (delta-alist (json-parse-string data :object-type 'alist)))
                         ;; The output text is given according to indexes; i
                         ;; don't yet really understand what different indexes
                         ;; mean for streaming, so let's just use index 0 for
                         ;; now.
                         (when (= (assoc-default 'content_index delta-alist) 0)
                           (funcall receiver `(:text ,(assoc-default 'delta delta-alist)))))))
                   (response.reasoning_summary_text.delta
                    .
                    ,(lambda (event)
                       (let* ((data (plz-event-source-event-data event))
                              (delta-alist (json-parse-string data :object-type 'alist)))
                         (when (and (= (assoc-default 'output_index delta-alist) 0)
                                    (> (length (assoc-default 'delta delta-alist)) 0))
                           (funcall receiver `(:reasoning ,(assoc-default 'delta delta-alist)))))))
                   (response.output_item.done
                    .
                    ,(lambda (event)
                       (let* ((data (plz-event-source-event-data event))
                              (done-alist (json-parse-string data :object-type 'alist))
                              (item (assoc-default 'item done-alist)))
                         (pcase (assoc-default 'type item)
                           ("reasoning" (funcall receiver
                                                 `(:multi-turn
                                                   (:openai-reasoning-id ,(assoc-default 'id item)
                                                                         :openai-encrypted-reasoning ,(assoc-default 'encrypted_content item)))))
                           ("function_call" (funcall receiver
                                                     (list :tool-uses
                                                           (list (make-llm-provider-utils-tool-use
                                                                  :id (assoc-default 'id item)
                                                                  :name (assoc-default 'name item)
                                                                  :args (json-parse-string
                                                                         (assoc-default 'arguments item)
                                                                         :object-type 'alist))))))))))))))

(cl-defmethod llm-provider-collect-streaming-tool-uses ((_ llm-openai) data)
  data)

(cl-defmethod llm-provider-collect-streaming-tool-uses ((_ llm-openai-compatible) data)
  (llm-provider-utils-openai-collect-streaming-tool-uses data))

(cl-defmethod llm-name ((_ llm-openai))
  "Return the name of the provider."
  "Open AI")

(cl-defmethod llm-name ((provider llm-openai-compatible))
  "Return the name of the `llm-openai-compatible' PROVIDER."
  (or (when-let* ((model-id (llm-openai-primary-chat-model provider))
                  (model (llm-models-match model-id)))
        (llm-model-name model))
      "Open AI Compatible"))

(cl-defmethod llm-chat-token-limit ((provider llm-openai))
  (llm-provider-utils-model-token-limit (llm-openai-primary-chat-model provider)))

(cl-defmethod llm-capabilities ((provider llm-openai))
  (seq-uniq
   (append '(streaming embeddings tool-use streaming-tool-use json-response model-list)
           (when-let* ((model (llm-models-match (llm-openai-chat-model provider))))
             (seq-intersection (llm-model-capabilities model)
                               '(image-input reasoning))))))

(cl-defmethod llm-capabilities ((provider llm-openai-compatible))
  (append '(streaming model-list)
          (when (and (llm-openai-compatible-embedding-model provider)
                     (not (equal "unset" (llm-openai-compatible-embedding-model provider))))
            '(embeddings embeddings-batch))

          (when-let* ((model (llm-models-match (llm-openai-primary-chat-model provider))))
            (llm-model-capabilities model))))

(cl-defmethod llm-capabilities ((_ llm-openrouter))
  (seq-remove
   (lambda (c) (eq c 'embeddings-batch))
   (cl-call-next-method)))

(cl-defmethod llm-models ((provider llm-openai))
  (mapcar (lambda (model)
            (plist-get model :id))
          (append
           (plist-get (plz 'get (llm-openai--url provider "models")
                        :as (lambda () (json-parse-buffer :object-type 'plist))
                        :headers (llm-openai--headers provider))
                      :data)
           nil)))

(provide 'llm-openai)

;;; llm-openai.el ends here
