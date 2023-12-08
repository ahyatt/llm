;;; llm-vertex.el --- LLM implementation of Google Cloud Vertex AI -*- lexical-binding: t -*-

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
;; This file implements the llm functionality defined in llm.el, for Google
;; Cloud Vertex AI.

(require 'cl-lib)
(require 'llm)
(require 'llm-request)
(require 'json)

(defgroup llm-vertex nil
  "LLM implementation for Google Cloud Vertex AI."
  :group 'llm)

(defcustom llm-vertex-gcloud-binary "gcloud"
  "The executable to use for the gcloud binary.
If the binary is not in the PATH, the full path must be specified."
  :type 'file
  :group 'llm-vertex)

(defcustom llm-vertex-gcloud-region "us-central1"
  "The gcloud region to use to connect to Vertex AI."
  :type 'string
  :group 'llm-vertex)

(defcustom llm-vertex-example-prelude "Examples of how you should respond follow."
  "The prelude to use for examples in Vertex chat prompts.
This is only used for streaming calls."
  :type 'string
  :group 'llm-vertex)

(defcustom llm-vertex-default-max-output-tokens 500
  "The default maximum number of tokens to ask for.
This is only used when setting the maximum tokens is required,
and there is no default. The maximum value possible here is 2049."
  :type 'integer
  :group 'llm-vertex)

(defcustom llm-vertex-default-chat-model "chat-bison"
  "The default model to ask for.
This should almost certainly be a chat model, other models are
for more specialized uses."
  :type 'string
  :group 'llm-vertex)

(cl-defstruct llm-vertex
  "A struct representing a Vertex AI client.

KEY is the temporary API key for the Vertex AI. It is required to
be populated before any call.

CHAT-MODEL is the name of the chat model to use. If unset, will use a reasonable default.

EMBEDDING-MODEL is the name of the embedding model to use. If unset, will use a reasonable default.

KEY-GENTIME keeps track of when the key was generated, because the key must be regenerated every hour."
  key
  project
  embedding-model
  (chat-model llm-vertex-default-chat-model)
  key-gentime)

(defun llm-vertex-refresh-key (provider)
  "Refresh the key in the vertex PROVIDER, if needed."
  (unless (and (llm-vertex-key provider)
               (> (* 60 60)
                  (float-time (time-subtract (current-time) (or (llm-vertex-key-gentime provider) 0)))))
    (let ((result (string-trim (shell-command-to-string (concat llm-vertex-gcloud-binary " auth print-access-token")))))
      (when (string-match-p "ERROR" result)
        (error "Could not refresh gcloud access token, received the following error: %s" result))
      ;; We need to make this unibyte, or else it doesn't causes problems when
      ;; the user is using multibyte strings.
      (setf (llm-vertex-key provider) (encode-coding-string result 'utf-8)))
    (setf (llm-vertex-key-gentime provider) (current-time))))

(cl-defmethod llm-nonfree-message-info ((provider llm-vertex))
  (ignore provider)
  (cons "Google Cloud Vertex" "https://policies.google.com/terms/generative-ai"))

(defun llm-vertex--embedding-url (provider)
  "From the PROVIDER, return the URL to use for embeddings"
  (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:predict"
                             llm-vertex-gcloud-region
                             (llm-vertex-project provider)
                             llm-vertex-gcloud-region
                             (or (llm-vertex-embedding-model provider) "textembedding-gecko")))

(defun llm-vertex--embedding-extract-response (response)
  "Return the embedding contained in RESPONSE."
  (cdr (assoc 'values (cdr (assoc 'embeddings (aref (cdr (assoc 'predictions response)) 0))))))

(defun llm-vertex--error-message (err-response)
  "Return a user-visible error message from ERR-RESPONSE."
  (format "Problem calling GCloud Vertex AI: status: %s message: %s (%s)"
          (assoc-default 'status (assoc-default 'error err-response))
          (assoc-default 'message (assoc-default 'error err-response))
          err-response))

(defun llm-vertex--handle-response (response extractor)
  "If RESPONSE is an error, throw it, else call EXTRACTOR."
  (if (assoc 'error response)
      (error (llm-vertex--error-message response))
    (funcall extractor response)))

(cl-defmethod llm-embedding-async ((provider llm-vertex) string vector-callback error-callback)
  (llm-vertex-refresh-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-vertex--embedding-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data `(("instances" . [(("content" . ,string))]))
                     :on-success (lambda (data)
                                   (llm-request-callback-in-buffer
                                    buf vector-callback (llm-vertex--embedding-extract-response data)))
                     :on-error (lambda (_ data)
                                 (llm-request-callback-in-buffer
                                  buf error-callback
                                  'error (llm-vertex--error-message data))))))

(cl-defmethod llm-embedding ((provider llm-vertex) string)
  (llm-vertex-refresh-key provider)
  (llm-vertex--handle-response
   (llm-request-sync (llm-vertex--embedding-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data `(("instances" . [(("content" . ,string))])))
   #'llm-vertex--embedding-extract-response))

(defun llm-vertex--parameters-ui (prompt)
  "Return a alist setting parameters, appropriate for the ui API.
If nothing needs to be set, return nil."
  (let ((param-struct-alist))
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . (("float_val" . ,(llm-chat-prompt-temperature prompt)))) param-struct-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("maxOutputTokens" . (("int_val" . ,(llm-chat-prompt-max-tokens prompt)))) param-struct-alist))
    ;; Wrap in the "parameters" and "struct_val" keys
    (if param-struct-alist
        `(("parameters" . (("struct_val" . ,param-struct-alist)))))))

(defun llm-vertex--get-chat-response-streaming (response)
  "Return the actual response from the RESPONSE struct returned.
This handles different kinds of models."
  (pcase (type-of response)
    ('vector (mapconcat #'llm-vertex--get-chat-response-streaming
                        response ""))
    ('cons (let* ((outputs (assoc-default 'outputs response))
                  (structVal-list (assoc-default 'structVal (aref outputs 0)))
                  (candidates (assoc-default 'candidates structVal-list)))
             (if candidates
                 (let* ((listVal (assoc-default 'listVal candidates))
                        (structVal (assoc-default 'structVal (aref listVal 0)))
                        (content (assoc-default 'content structVal))
                        (stringVal (aref (assoc-default 'stringVal content) 0)))
                   stringVal)
               (aref (assoc-default 'stringVal (assoc-default 'content structVal-list)) 0))))))

(defun llm-vertex--get-partial-chat-ui-repsonse (response)
  "Return the partial response from as much of RESPONSE as we can parse.
If the response is not parseable, return nil."
  (with-temp-buffer
    (insert response)
    (let ((start (point-min))
          (end-of-valid-chunk
           (save-excursion
             (goto-char (point-max))
             (search-backward "\n," nil t)
             (point))))
      (when (and start end-of-valid-chunk)
        ;; It'd be nice if our little algorithm always worked, but doesn't, so let's
        ;; just ignore when it fails.  As long as it mostly succeeds, it should be fine.
        (condition-case nil
            (when-let
                ((json (ignore-errors
                        (json-read-from-string
                         (concat
                          (buffer-substring-no-properties
                           start end-of-valid-chunk)
                          ;; Close off the json
                          "]")))))
              (llm-vertex--get-chat-response-streaming json))
          (error (message "Unparseable buffer saved to *llm-vertex-unparseable*")
                 (with-current-buffer (get-buffer-create "*llm-vertex-unparseable*")
                     (erase-buffer)
                     (insert response))))))))

(defun llm-vertex--collapsed-system-prompt (prompt)
  "Return the text of the non-interaction parts of PROMPT.
If there are no non-interaction parts, return nil."
  (let ((system-prompt))
    (when (llm-chat-prompt-context prompt)
      (push (llm-chat-prompt-context prompt) system-prompt))
    (when (llm-chat-prompt-examples prompt)
      (push (concat llm-vertex-example-prelude "\n"
                    (mapconcat (lambda (example)
                                 (concat "User:\n" (car example) "\nAssistant:\n" (cdr example)))
                               (llm-chat-prompt-examples prompt) "\n"))
            system-prompt))
    (when system-prompt
      (mapconcat #'identity (nreverse system-prompt) "\n"))))

(defun llm-vertex--chat-request-streaming (prompt model)
  "Return an alist with chat input for the streaming API.
PROMPT contains the input to the call to the chat API. MODEL
contains the model to use, which can change the request."
  (let ((system-prompt (llm-vertex--collapsed-system-prompt prompt)))    
    (append
     `(("inputs" . ((("struct_val" .
                      ,(if (string-match-p "text-bison" model)
                          (progn
                            (unless (= 1 (length (llm-chat-prompt-interactions prompt)))
                              (error "Vertex model 'text-bison' must contain only one interaction"))
                            `(("prompt" . (("string_val" .
                                           [,(format "'\"%s\"'"
                                                     (concat system-prompt (when system-prompt "\n")
                                                             (llm-chat-prompt-interaction-content
                                                              (car (llm-chat-prompt-interactions prompt )))))])))))
                         `(("messages" .
                            (("list_val" .
                              ,(mapcar (lambda (interaction)
                                         `(("struct_val" . (("content" .
                                                             (("string_val" .
                                                               (,(format "'\"%s\"'"
                                                                         (llm-chat-prompt-interaction-content
                                                                          interaction))))))
                                                            ("author" .
                                                             (("string_val" .
                                                               ,(format "'\"%s\"'"
                                                                        (pcase (llm-chat-prompt-interaction-role interaction)
                                                                          ('user "user")
                                                                          ('system "system")
                                                                          ('assistant "assistant"))))))))))
                                       ;; Only append the system prompt if this is the first message of the conversation.
                                       (if (and system-prompt (= (length (llm-chat-prompt-interactions prompt)) 1))
                                           (cons (make-llm-chat-prompt-interaction
                                                  :role 'user
                                                  :content (concat system-prompt (llm-chat-prompt-interaction-content
                                                                                   (car (llm-chat-prompt-interactions prompt)))))
                                                 (cdr (llm-chat-prompt-interactions prompt)))
                                         (llm-chat-prompt-interactions prompt)))))))))))))
     (llm-vertex--parameters-ui prompt))))

(defun llm-vertex--chat-parameters (prompt)
  "From PROMPT, create the parameters section.
Return value is a cons for adding to an alist, unless there is
nothing to add, in which case it is nil."
  (let ((params-alist))
    (when (llm-chat-prompt-temperature prompt)
            (push `(temperature . ,(llm-chat-prompt-temperature prompt))
                  params-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `(maxOutputTokens . ,(llm-chat-prompt-max-tokens prompt)) params-alist))
    (when params-alist
      `(parameters . ,params-alist))))

(defun llm-vertex--text-request (prompt)
  "From PROMPT, create the data for the vertex text reequest.
The text request can only have one interaction."
  (unless (= 1 (length (llm-chat-prompt-interactions prompt)))
    (error "Model text-bison can only have 1 prompt interaction"))
  (let ((system-prompt (llm-vertex--collapsed-system-prompt prompt)))
    (append
     `((instances . [((prompt . ,(concat system-prompt
                                        (when system-prompt "\n")
                                        (llm-chat-prompt-interaction-content
                                              (car (llm-chat-prompt-interactions prompt))))))]))
     (let ((params (llm-vertex--chat-parameters (let ((p (copy-llm-chat-prompt prompt)))
                                                  ;; For some reason vertex requires max-tokens
                                                  (setf (llm-chat-prompt-max-tokens p)
                                                        llm-vertex-default-max-output-tokens)
                                                  p))))
       (when params (list params))))))

(defun llm-vertex--chat-request-v1 (prompt model)
  "From PROMPT, create the data for the vertex chat request."
  (if (string-match-p "text-bison" model)
      (llm-vertex--text-request prompt)
    (let ((prompt-alist))
      (when (llm-chat-prompt-context prompt)
        (push `("context" . ,(llm-chat-prompt-context prompt)) prompt-alist))
      (when (llm-chat-prompt-examples prompt)
        (push `("examples" . ,(apply #'vector
                                     (mapcar (lambda (example)
                                        `(("input" . (("content" . ,(car example))))
                                          ("output" . (("content" . ,(cdr example))))))
                                             (llm-chat-prompt-examples prompt))))
              prompt-alist))
      (push `("messages" . ,(apply #'vector
                                   (mapcar (lambda (interaction)
                                             `(("author" . (pcase (llm-chat-prompt-interaction-role interaction)
                                                             ('user "user")
                                                             ('system (error "System role not supported"))
                                                             ('assistant "assistant")))
                                               ("content" . ,(llm-chat-prompt-interaction-content interaction))))
                                           (llm-chat-prompt-interactions prompt))))
            prompt-alist)      
      (append
        `(("instances" . [,prompt-alist]))
        (let ((params (llm-vertex--chat-parameters prompt)))
          (when params (list params)))))))

(defun llm-vertex--chat-url (provider streaming)
"Return the correct url to use for PROVIDER.
If STREAMING is non-nil, use the URL for the streaming API."
  (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:%s"
          llm-vertex-gcloud-region
          (llm-vertex-project provider)
          llm-vertex-gcloud-region
          (llm-vertex-chat-model provider)
          (if streaming "serverStreamingPredict" "predict")))

(defun llm-vertex--chat-extract-response (response)
  "Return the chat response contained in the server RESPONSE.
This should handle the various kinds of responses that the
different models can return."
  (let* ((predictions (aref (assoc-default 'predictions response) 0))
         (candidates (assoc-default 'candidates predictions)))
    (if candidates
        (assoc-default 'content (aref candidates 0))
      (assoc-default 'content predictions))))

(cl-defmethod llm-chat-async ((provider llm-vertex) prompt response-callback error-callback)
  (llm-vertex-refresh-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-vertex--chat-url provider nil)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data (llm-vertex--chat-request-v1 prompt (llm-vertex-chat-model provider))
                     :on-success (lambda (data)
                                   (let ((response (llm-vertex--chat-extract-response data)))
                                     (setf (llm-chat-prompt-interactions prompt)
                                           (append (llm-chat-prompt-interactions prompt)
                                                   (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
                                     (llm-request-callback-in-buffer buf response-callback response)))
                     :on-error (lambda (_ data)
                                 (llm-request-callback-in-buffer buf error-callback 'error
                                          (llm-vertex--error-message data))))))

(cl-defmethod llm-chat ((provider llm-vertex) prompt)
  (llm-vertex-refresh-key provider)
  (let ((response (llm-vertex--handle-response
                 (llm-request-sync
                  (llm-vertex--chat-url provider nil)
                  :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                  :data (llm-vertex--chat-request-v1 prompt (llm-vertex-chat-model provider)))
                 #'llm-vertex--chat-extract-response)))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
    response))

;; API reference: https://cloud.google.com/vertex-ai/docs/generative-ai/learn/streaming
(cl-defmethod llm-chat-streaming ((provider llm-vertex) prompt partial-callback response-callback error-callback)
  (llm-vertex-refresh-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-vertex--chat-url provider t)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data (llm-vertex--chat-request-streaming prompt (llm-vertex-chat-model provider))
                     :on-partial (lambda (partial)
                                   (when-let ((response (llm-vertex--get-partial-chat-ui-repsonse partial)))
                                     (llm-request-callback-in-buffer buf partial-callback response)))
                     :on-success (lambda (data)
                                   (let ((response (llm-vertex--get-chat-response-streaming data)))
                                     (setf (llm-chat-prompt-interactions prompt)
                                           (append (llm-chat-prompt-interactions prompt)
                                                   (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
                                     (llm-request-callback-in-buffer buf response-callback response)))
                     :on-error (lambda (_ data)
                                 (llm-request-callback-in-buffer buf error-callback 'error
                                                                 (llm-vertex--error-message data))))))

;; Token counts
;; https://cloud.google.com/vertex-ai/docs/generative-ai/get-token-count

(defun llm-vertex--count-token-url (provider)
  "Return the URL to use for the Vertex API.
PROVIDER is the llm provider.
MODEL "
  (format "https://%s-aiplatform.googleapis.com/v1beta1/projects/%s/locations/%s/publishers/google/models/%s:countTokens"
          llm-vertex-gcloud-region
          (llm-vertex-project provider)
          llm-vertex-gcloud-region
          (llm-vertex-chat-model provider)))

(defun llm-vertex--to-count-token-request (request)
  "Return a version of REQUEST that is suitable for counting tokens."
  (seq-filter (lambda (c) (and (not (equal (car c) "parameters"))
                               (not (eq (car c) 'parameters)))) request))

(defun llm-vertex--count-tokens-extract-response (response)
  "Extract the token count from the response."
  (assoc-default 'totalTokens response))

(cl-defmethod llm-count-tokens ((provider llm-vertex) string)
  (llm-vertex-refresh-key provider)
  (llm-vertex--handle-response
   (llm-request-sync (llm-vertex--count-token-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data (llm-vertex--to-count-token-request
                            (llm-vertex--chat-request-v1
                             (llm-make-simple-chat-prompt string) (llm-vertex-chat-model provider))))
   #'llm-vertex--count-tokens-extract-response))

(provide 'llm-vertex)

;;; llm-vertex.el ends here
