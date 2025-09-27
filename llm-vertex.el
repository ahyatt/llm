;;; llm-vertex.el --- LLM implementation of Google Cloud Vertex AI -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023-2025  Free Software Foundation, Inc.

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

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-request-plz)
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
and there is no default.  The maximum value possible here is 2049."
  :type 'integer
  :group 'llm-vertex)

(defcustom llm-vertex-default-chat-model "gemini-2.5-pro"
  "The default model to ask for.
This should almost certainly be a chat model, other models are
for more specialized uses."
  :type 'string
  :group 'llm-vertex)

(cl-defstruct (llm-google (:include llm-standard-full-provider))
  "A base class for functionality that is common to both Vertex and
Gemini.")

(cl-defstruct (llm-vertex (:include llm-google))
  "A struct representing a Vertex AI client.

KEY is the temporary API key for the Vertex AI.  It is required to
be populated before any call.

CHAT-MODEL is the name of the chat model to use.  If unset, will
use a reasonable default.

EMBEDDING-MODEL is the name of the embedding model to use.  If
unset, will use a reasonable default.

KEY-GENTIME keeps track of when the key was generated, because
the key must be regenerated every hour."
  key
  project
  embedding-model
  (chat-model llm-vertex-default-chat-model)
  key-gentime)

;; API reference: https://cloud.google.com/vertex-ai/docs/generative-ai/multimodal/send-chat-prompts-gemini#gemini-chat-samples-drest

(cl-defmethod llm-provider-request-prelude ((provider llm-vertex))
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

(cl-defmethod llm-provider-embedding-url ((provider llm-vertex) &optional _)
  (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:predict"
          llm-vertex-gcloud-region
          (llm-vertex-project provider)
          llm-vertex-gcloud-region
          (or (llm-vertex-embedding-model provider) "textembedding-gecko")))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-vertex) response)
  (assoc-default 'values (assoc-default 'embeddings (aref (assoc-default 'predictions response) 0))))

(cl-defmethod llm-provider-embedding-extract-error ((provider llm-google) err-response)
  (llm-provider-chat-extract-error provider err-response))

(cl-defmethod llm-provider-chat-extract-error ((provider llm-google) err-response)
  (if (vectorp err-response)
      (llm-provider-chat-extract-error provider (aref err-response 0))
    (if-let ((err (assoc-default 'error err-response)))
        (format "Problem calling GCloud Vertex AI: status: %s message: %s"
                (assoc-default 'code err)
                (assoc-default 'message err))
      (if-let ((candidates (assoc-default 'candidates err-response)))
          (when (and (vectorp candidates)
                     (> (length candidates) 0)
                     (equal "SAFETY"
                            (assoc-default 'finishReason (aref candidates 0))))
            (format "Could not finish due to detected Gemini safety violations: %s"
                    (assoc-default 'safetyRatings (aref candidates 0))))))))

(cl-defmethod llm-provider-embedding-request ((_ llm-vertex) string)
  `(:instances [(:content ,string)]))

(cl-defmethod llm-provider-headers ((provider llm-vertex))
  `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider)))))

(cl-defmethod llm-provider-chat-extract-result ((provider llm-google) response)
  (pcase (type-of response)
    ('vector (when (> (length response) 0)
               (let ((parts (mapcar (lambda (part) (llm-provider-chat-extract-result provider part))
                                    response)))
                 (if (stringp (car parts))
                     (mapconcat #'identity parts "")
                   (car parts)))))
    ('cons (if (assoc-default 'candidates response)
               (let ((parts (assoc-default
                             'parts
                             (assoc-default 'content
                                            (aref (assoc-default 'candidates response) 0)))))
                 (when parts
                   (assoc-default 'text (seq-find (lambda (part)
                                                    (not (assoc-default 'thought part)))
                                                  parts))))))))

(cl-defmethod llm-provider-extract-reasoning ((provider llm-google) response)
  (if (vectorp response)
      (llm-provider-extract-reasoning provider (aref response 0))
    ;; In some error cases, the response does not have any candidates.
    (when (assoc-default 'candidates response)
      (when-let* ((parts (assoc-default
                          'parts (assoc-default
                                  'content
                                  (aref (assoc-default 'candidates response) 0))))
                  (thought-part (seq-find (lambda (part)
                                            (assoc-default 'thought part))
                                          parts)))
        (assoc-default 'text thought-part)))))

(cl-defmethod llm-provider-extract-tool-uses ((provider llm-google) response)
  (if (vectorp response)
      (llm-provider-extract-tool-uses provider (aref response 0))
    ;; In some error cases, the response does not have any candidates.
    (when (assoc-default 'candidates response)
      (mapcar (lambda (call)
                (make-llm-provider-utils-tool-use
                 :name (assoc-default 'name call)
                 :args (assoc-default 'args call)))
              (mapcan (lambda (maybe-call)
                        (when-let ((fc (assoc-default 'functionCall maybe-call)))
                          (list fc)))
                      (assoc-default
                       'parts (assoc-default
                               'content
                               (aref (assoc-default 'candidates response) 0))))))))

(defun llm-vertex--interaction (interaction)
  "Return the interaction from INTERACTION to be used in the request."
  `(:role ,(pcase (llm-chat-prompt-interaction-role interaction)
             ('user "user")
             ('assistant "model")
             ('tool-results "function"))
          :parts
          ,(cond
            ((eq 'tool-results (llm-chat-prompt-interaction-role interaction))
             (vconcat
              (mapcar (lambda (fc)
                        `(:functionResponse
                          (:name ,(llm-chat-prompt-tool-result-tool-name fc)
                                 :response
                                 (:name ,(llm-chat-prompt-tool-result-tool-name fc)
                                        :content ,(llm-chat-prompt-tool-result-result fc)))))
                      (llm-chat-prompt-interaction-tool-results interaction))))
            ((and (consp (llm-chat-prompt-interaction-content interaction))
                  (llm-provider-utils-tool-use-p (car (llm-chat-prompt-interaction-content interaction))))
             (vconcat
              (mapcar (lambda (tool-use)
                        `(:functionCall
                          (:name ,(llm-provider-utils-tool-use-name tool-use)
                                 :args ,(llm-provider-utils-tool-use-args tool-use))))
                      (llm-chat-prompt-interaction-content interaction))))
            ((llm-multipart-p (llm-chat-prompt-interaction-content interaction))
             (vconcat (mapcar (lambda (part)
                                (if (llm-media-p part)
                                    `(:inline_data
                                      (:mime_type ,(llm-media-mime-type part)
                                                  :data ,(base64-encode-string (llm-media-data part) t)))
                                  `(:text ,part)))
                              (llm-multipart-parts (llm-chat-prompt-interaction-content interaction)))))
            (t `[(:text ,(llm-chat-prompt-interaction-content interaction))]))))

(defun llm-provider--chat-request (prompt model)
  "Create the request for the chat PROMPT and MODEL symbol."
  (llm-provider-utils-combine-to-system-prompt prompt llm-vertex-example-prelude)
  (append
   (when (eq 'system (llm-chat-prompt-interaction-role (car (llm-chat-prompt-interactions prompt))))
     `(:system_instruction
       (:parts (:text ,(llm-chat-prompt-interaction-content
                        (car (llm-chat-prompt-interactions prompt)))))))
   `(:contents
     ,(vconcat (mapcan (lambda (interaction)
                         (unless (eq 'system (llm-chat-prompt-interaction-role interaction))
                           (list (llm-vertex--interaction interaction))))
                       (llm-chat-prompt-interactions prompt))))
   (when (llm-chat-prompt-tools prompt)
     ;; Although Gemini claims to be compatible with Open AI's function declaration,
     ;; it's only somewhat compatible.
     `(:tools
       [(:function_declarations
         ,(vconcat (mapcar
                    (lambda (tool)
                      `(:name ,(llm-tool-name tool)
                              :description ,(llm-tool-description tool)
                              :parameters ,(llm-provider-utils-openai-arguments
                                            (llm-tool-args tool))))
                    (llm-chat-prompt-tools prompt))))]))
   (llm-vertex--chat-parameters prompt model)))

;; TODO: remove after September 2025, this is only here so people can upgrade to
;; a new version of their llm library without the old llm-google specializer
;; sticking around.
(cl-defmethod llm-provider-chat-request ((_ llm-google) _ _)
  (cl-call-next-method))

(defun llm-vertex-transform-response-format (format)
  "Transform FORMAT plist into the appropriate Vertex response schema."
  (let (result)
    (map-do (lambda (k v)
              (when (member k '(:type :format :description :nullable :enum :maxItems
                                      :minItems :properties :required :propertyOrdering :items))
                (setq result (plist-put result k
                                        (cond
                                         ((eq k :properties)
                                          (let (inner-plist)
                                            (map-do (lambda (prop prep-def)
                                                      (setq inner-plist (plist-put inner-plist
                                                                                   prop
                                                                                   (llm-vertex-transform-response-format prep-def))))
                                                    v)
                                            inner-plist))
                                         ((eq k :items)
                                          (llm-vertex-transform-response-format v))
                                         (t v))))))
            format)
    result))

(cl-defmethod llm-provider-chat-request ((provider llm-vertex) prompt _)
  (llm-provider--chat-request prompt (let ((model (llm-models-match (llm-vertex-chat-model provider))))
                                       (if model
                                           (llm-model-symbol model)
                                         'unknown))))

(defun llm-vertex--chat-parameters (prompt model)
  "From PROMPT, create the parameters section.
Return value is a cons for adding to an alist, unless there is nothing
to add, in which case it is nil.  MODEL is the symbol of the model used,
which is necessary to properly set some paremeters."
  (let ((params-plist (llm-provider-utils-non-standard-params-plist prompt)))
    (when (llm-chat-prompt-temperature prompt)
      (setq params-plist (plist-put params-plist :temperature
                                    (* (llm-chat-prompt-temperature prompt) 2.0))))
    (when (llm-chat-prompt-max-tokens prompt)
      (setq params-plist (plist-put params-plist :maxOutputTokens
                                    (llm-chat-prompt-max-tokens prompt))))
    (when-let ((format (llm-chat-prompt-response-format prompt)))
      (setq params-plist (plist-put params-plist :response_mime_type
                                    "application/json"))
      (unless (eq 'json format)
        (setq params-plist (plist-put params-plist :response_schema
                                      (llm-vertex-transform-response-format
                                       (llm-provider-utils-convert-to-serializable format))))))
    (let* (thinking-plist
           (model-data (llm-models-by-symbol model)))
      (when (and model-data (member 'reasoning (llm-model-capabilities model-data)))
        (setq thinking-plist '(:includeThoughts t))
        (when-let ((budget (llm-chat-prompt-reasoning prompt))
                   (max-budget (if (eq model 'gemini-2.5-pro) 32768 24576)))
          (if (and (eq model 'gemini-2.5-pro) (eq budget 'none))
              (display-warning 'llm :warning "Cannot turn off reasoning in Gemini 2.5 Pro, ignoring reasoning setting")
            (setq thinking-plist (plist-put thinking-plist
                                            :thinkingBudget
                                            (pcase budget
                                              ('none 0)
                                              ('light 1024)
                                              ('medium (/ max-budget 2))
                                              ('maximum max-budget)))))))
      (when thinking-plist
        (setq params-plist (plist-put params-plist :thinkingConfig thinking-plist))))
    (when params-plist
      `(:generationConfig ,params-plist))))

(cl-defmethod llm-provider-populate-tool-uses ((_ llm-google) prompt tool-uses)
  (llm-provider-utils-append-to-prompt prompt tool-uses nil 'assistant))

(cl-defmethod llm-provider-streaming-media-handler ((provider llm-google)
                                                    receiver err-receiver)
  (cons 'application/json
        (plz-media-type:application/json-array
         :handler
         (lambda (element)
           (when-let ((err-response (llm-provider-chat-extract-error provider element)))
             (funcall err-receiver err-response))
           (if-let ((response (llm-provider-chat-extract-result provider element)))
               (funcall receiver `(:text ,response))
             (when-let ((fc (llm-provider-extract-tool-uses provider element)))
               (funcall receiver `(:tool-uses ,fc))))))))

(cl-defmethod llm-provider-collect-streaming-tool-uses ((_ llm-google) data)
  (car data))

(defun llm-vertex--chat-url (provider &optional streaming)
  "Return the correct url to use for PROVIDER.
If STREAMING is non-nil, use the URL for the streaming API."
  (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:%s"
          llm-vertex-gcloud-region
          (llm-vertex-project provider)
          llm-vertex-gcloud-region
          (llm-vertex-chat-model provider)
          (if streaming "streamGenerateContent" "generateContent")))

(cl-defmethod llm-provider-chat-url ((provider llm-vertex))
  (llm-vertex--chat-url provider))

(cl-defmethod llm-provider-chat-streaming-url ((provider llm-vertex))
  (llm-vertex--chat-url provider t))

(cl-defmethod llm-name ((_ llm-vertex))
  "Return the name of the provider."
  "Vertex Gemini")

(cl-defmethod llm-chat-token-limit ((provider llm-vertex))
  (llm-provider-utils-model-token-limit (llm-vertex-chat-model provider)))

(cl-defmethod llm-capabilities ((provider llm-vertex))
  (append
   (list 'streaming 'embeddings 'json-response)
   (when-let ((model (llm-models-match (llm-vertex-chat-model provider)))
              (capabilities (llm-model-capabilities model)))
     (append
      (when (member 'tool-use capabilities) '(tool-uses streaming-tool-uses))
      (seq-intersection capabilities '(image-input audio-input video-input))))))

(provide 'llm-vertex)

;;; llm-vertex.el ends here
