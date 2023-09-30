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
  chat-model
  key-gentime)

(defun llm-vertex-refresh-key (provider)
  "Refresh the key in the vertex PROVIDER, if needed."
  (unless (and (llm-vertex-key provider)
               (> (* 60 60)
                  (float-time (time-subtract (current-time) (or (llm-vertex-key-gentime provider) 0)))))
    (let ((result (string-trim (shell-command-to-string (concat llm-vertex-gcloud-binary " auth print-access-token")))))
      (when (string-match-p "ERROR" result)
        (error "Could not refresh gcloud access token, received the following error: %s" result))
      (setf (llm-vertex-key provider) result))
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
  (llm-request-async (llm-vertex--embedding-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data `(("instances" . [(("content" . ,string))]))
                     :on-success (lambda (data)
                                   (funcall vector-callback (llm-vertex--embedding-extract-response data)))
                     :on-error (lambda (_ data)
                                 (funcall error-callback
                                          'error
                                          (llm-vertex--error-message data)))))

(cl-defmethod llm-embedding ((provider llm-vertex) string)
  (llm-vertex-refresh-key provider)
  (llm-vertex--handle-response
   (llm-request-sync (llm-vertex--embedding-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data `(("instances" . [(("content" . ,string))])))
   #'llm-vertex--embedding-extract-response))

(defun llm-vertex--chat-request (prompt)
  "From PROMPT, create the data to in the vertex chat request."
  (let ((prompt-alist)
        (params-alist))
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
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt))
            params-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("max_tokens" . ,(llm-chat-prompt-max-tokens prompt)) params-alist))
    `(("instances" . [,prompt-alist])
      ("parameters" . ,params-alist))))

(defun llm-vertex--chat-url (provider)
  "Return the correct url to use for PROVIDER."
  (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:predict"
                                   llm-vertex-gcloud-region
                                   (llm-vertex-project provider)
                                   llm-vertex-gcloud-region
                                   (or (llm-vertex-chat-model provider) "chat-bison")))

(defun llm-vertex--chat-extract-response (response)
  "Return the chat response contained in the server RESPONSE."
  (cdr (assoc 'content (aref (cdr (assoc 'candidates (aref (cdr (assoc 'predictions response)) 0))) 0))))

(cl-defmethod llm-chat-async ((provider llm-vertex) prompt response-callback error-callback)
  (llm-vertex-refresh-key provider)
  (llm-request-async (llm-vertex--chat-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data (llm-vertex--chat-request prompt)
                     :on-success (lambda (data)
                                   (funcall response-callback (llm-vertex--chat-extract-response data)))
                     :on-error (lambda (_ data)
                                 (funcall error-callback 'error
                                          (llm-vertex--error-message data)))))

(cl-defmethod llm-chat ((provider llm-vertex) prompt)
  (llm-vertex-refresh-key provider)
  (llm-vertex--handle-response
   (llm-request-sync (llm-vertex--chat-url provider)
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider))))
                     :data (llm-vertex--chat-request prompt))
   #'llm-vertex--chat-extract-response))

(provide 'llm-vertex)

;;; llm-vertex.el ends here
