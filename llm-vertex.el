;;; llm-vertex.el --- LLM implementation of Google Cloud Vertex AI -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
(require 'llm-provider-utils)
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

(defcustom llm-vertex-default-chat-model "gemini-pro"
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
  (let ((err (assoc-default 'error err-response)))
    (format "Problem calling GCloud Vertex AI: status: %s message: %s"
            (assoc-default 'code err)
            (assoc-default 'message err))))

(defun llm-vertex--handle-response (response extractor)
  "If RESPONSE is an errorp, throw it, else call EXTRACTOR."
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

(defun llm-vertex--get-chat-response (response)
  "Return the actual response from the RESPONSE struct returned.
This handles different kinds of models."
  (pcase (type-of response)
    ('vector (when (> (length response) 0)
               (let ((parts (mapcar #'llm-vertex--get-chat-response response)))
                 (if (stringp (car parts))
                     (mapconcat #'identity parts "")
                   (car parts)))))
    ('cons (if (assoc-default 'candidates response)
               (let ((parts (assoc-default
                             'parts
                             (assoc-default 'content
                                            (aref (assoc-default 'candidates response) 0)))))
                 (if parts
                      (or (assoc-default 'text (aref parts 0))
                         ;; Change function calling from almost Open AI's
                         ;; standard format to exactly the format.
                         (mapcar (lambda (call)
                                   `(function . ,(mapcar (lambda (c) (if (eq (car c) 'args) (cons 'arguments (cdr c)) c))
                                                         (cdar call))))
                                 parts))
                   ""))
             "NOTE: No response was sent back by the LLM, the prompt may have violated safety checks."))))

(defun llm-vertex--get-partial-chat-response (response)
  "Return the partial response from as much of RESPONSE as we can parse."
  (with-temp-buffer
    (insert response)
    (let ((result ""))
      ;; We just will parse every line that is "text": "..." and concatenate them.   
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (rx (seq (literal "\"text\": ")
                                           (group-n 1 ?\" (* any) ?\") line-end)) nil t)
          (setq result (concat result (json-read-from-string (match-string 1))))))
      result)))

(defun llm-vertex--chat-request (prompt)
  "Return an alist with chat input for the streaming API.
PROMPT contains the input to the call to the chat API."
  (llm-provider-utils-combine-to-user-prompt prompt llm-vertex-example-prelude)
  (append
   `((contents
      .
      ,(mapcar (lambda (interaction)
                 `((role . ,(pcase (llm-chat-prompt-interaction-role interaction)
                              ('user "user")
                              ('assistant "model")
                              ('function "function")))
                   (parts .
                          ,(if (and (not (equal (llm-chat-prompt-interaction-role interaction)
                                                'function))
                                    (stringp (llm-chat-prompt-interaction-content interaction)))
                               `(((text . ,(llm-chat-prompt-interaction-content
                                           interaction))))
                             (if (eq 'function
                                     (llm-chat-prompt-interaction-role interaction))
                                 (let ((fc (llm-chat-prompt-interaction-function-call-result interaction)))
                                   `(((functionResponse
                                       .
                                       ((name . ,(llm-chat-prompt-function-call-result-function-name fc))
                                        (response
                                         .
                                         ((name . ,(llm-chat-prompt-function-call-result-function-name fc))
                                          (content . ,(llm-chat-prompt-function-call-result-result fc)))))))))
                               (llm-chat-prompt-interaction-content interaction))))))
               (llm-chat-prompt-interactions prompt))))
   (when (llm-chat-prompt-functions prompt)
     ;; Although Gemini claims to be compatible with Open AI's function declaration,
     ;; it's only somewhat compatible.
     `(("tools" .
        ,(mapcar (lambda (tool)
                   `((function_declarations . (((name . ,(llm-function-call-name tool))
                                                (description . ,(llm-function-call-description tool))
                                                (parameters
                                                 .
                                                 ,(llm-provider-utils-openai-arguments
                                                   (llm-function-call-args tool))))))))
                           (llm-chat-prompt-functions prompt)))))
   (llm-vertex--chat-parameters prompt)))

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
      `((generation_config . ,params-alist)))))

(defun llm-vertex--normalize-function-calls (response)
  "If RESPONSE has function calls, transform them to our common format."
  (if (consp response)
      (mapcar (lambda (f)
                (make-llm-provider-utils-function-call
                 :name (assoc-default 'name (cdr f))
                 :args (assoc-default 'arguments (cdr f))))
              response)
    response))

(cl-defmethod llm-provider-utils-populate-function-calls ((_ llm-vertex) prompt calls)
  (llm-provider-utils-append-to-prompt
   prompt
   ;; For Vertex there is just going to be one call
   (mapcar (lambda (fc)
             `((functionCall
                .
                ((name . ,(llm-provider-utils-function-call-name fc))
                 (args . ,(llm-provider-utils-function-call-args fc))))))
           calls)))

(defun llm-vertex--process-and-return (provider prompt response &optional error-callback)
  "Process RESPONSE from the PROVIDER.

This returns the response to be given to the client.

Any functions will be executed.

The response will be added to PROMPT.

Provider is the llm provider, for logging purposes.

ERROR-CALLBACK is called when an error is detected."
  (if (and (consp response)
           (assoc-default 'error response))
      (progn
        (when error-callback
          (funcall error-callback 'error (llm-vertex--error-message response)))
        response))
  (let ((return-val
         (llm-provider-utils-process-result
          provider prompt
          (llm-vertex--normalize-function-calls
           (llm-vertex--get-chat-response response)))))
    return-val))

(defun llm-vertex--chat-url (provider &optional streaming)
"Return the correct url to use for PROVIDER.
If STREAMING is non-nil, use the URL for the streaming API."
  (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:%s"
          llm-vertex-gcloud-region
          (llm-vertex-project provider)
          llm-vertex-gcloud-region
          (llm-vertex-chat-model provider)
          (if streaming "streamGenerateContent" "generateContent")))

;; API reference: https://cloud.google.com/vertex-ai/docs/generative-ai/multimodal/send-chat-prompts-gemini#gemini-chat-samples-drest
(cl-defmethod llm-chat ((provider llm-vertex) prompt)
  ;; Gemini just has a streaming response, but we can just call it synchronously.
  (llm-vertex-refresh-key provider)
  (llm-vertex--process-and-return
     provider prompt
     (llm-request-sync (llm-vertex--chat-url provider)
                       :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                       :data (llm-vertex--chat-request prompt))))

(cl-defmethod llm-chat-async ((provider llm-vertex) prompt response-callback error-callback)
  (llm-vertex-refresh-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-vertex--chat-url provider)
                       :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                       :data (llm-vertex--chat-request prompt)
                       :on-success (lambda (data)
                                     (llm-request-callback-in-buffer
                                      buf response-callback
                                      (llm-vertex--process-and-return
                                       provider prompt data)))
                       :on-error (lambda (_ data)
                                   (llm-request-callback-in-buffer buf error-callback 'error
                                                                   (llm-vertex--error-message data))))))

(cl-defmethod llm-chat-streaming ((provider llm-vertex) prompt partial-callback response-callback error-callback)
  (llm-vertex-refresh-key provider)
  (let ((buf (current-buffer)))
    (llm-request-async (llm-vertex--chat-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data (llm-vertex--chat-request prompt)
                     :on-partial (lambda (partial)
                                   (when-let ((response (llm-vertex--get-partial-chat-response partial)))
                                     (when (> (length response) 0)
                                       (llm-request-callback-in-buffer buf partial-callback response))))
                     :on-success (lambda (data)
                                   (llm-request-callback-in-buffer
                                    buf response-callback
                                    (llm-vertex--process-and-return
                                     provider prompt data)))
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
                            (llm-vertex--chat-request
                             (llm-make-simple-chat-prompt string))))
   #'llm-vertex--count-tokens-extract-response))

(cl-defmethod llm-name ((_ llm-vertex))
  "Gemini")

(defun llm-vertex--chat-token-limit (model)
  "Get token limit for MODEL."
  (cond ((equal "gemini-pro" model) 30720)
        ((equal "gemini-pro-vision" model) 12288)
        ;; This shouldn't happen unless there's a new model, which could be a
        ;; smaller or larger model. We'll play it safe and choose a reasonable
        ;; number.
        (t 4096)))

(cl-defmethod llm-chat-token-limit ((provider llm-vertex))
  (llm-vertex--chat-token-limit (llm-vertex-chat-model provider)))

(cl-defmethod llm-capabilities ((_ llm-vertex))
  (list 'streaming 'embeddings 'function-calls))

(provide 'llm-vertex)

;;; llm-vertex.el ends here
