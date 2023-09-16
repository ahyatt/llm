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
(require 'request)
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

(defun llm-vertex--embedding (provider string vector-callback error-callback sync)
  "Get the embedding for STRING.
PROVIDER, VECTOR-CALLBACK, ERROR-CALLBACK are all the same as
`llm-embedding-async'. SYNC, when non-nil, will wait until the
response is available to return."
  (llm-vertex-refresh-key provider)
  (request (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:predict"
                               llm-vertex-gcloud-region
                               (llm-vertex-project provider)
                               llm-vertex-gcloud-region
                               (or (llm-vertex-embedding-model provider) "textembedding-gecko"))
    :sync sync
    :timeout 5
    :type "POST"
    :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider)))
               ("Content-Type" . "application/json"))
    :data (json-encode `(("instances" . [(("content" . ,string))])))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall vector-callback
                         (cdr (assoc 'values (cdr (assoc 'embeddings (aref (cdr (assoc 'predictions data)) 0))))))))
    :error (cl-function (lambda (&key error-thrown data &allow-other-keys)
                          (funcall error-callback
                                   (error (format "Problem calling GCloud AI: %s (%S)"
                                                  (cdr error-thrown) data)))))))

(cl-defmethod llm-embedding-async ((provider llm-vertex) string vector-callback error-callback)
  (llm-vertex--embedding provider string vector-callback error-callback nil))

(cl-defmethod llm-embedding ((provider llm-vertex) string)
  (let ((response))
    (llm-vertex--embedding provider string
                           (lambda (vector) (setq response vector))
                           (lambda (_ error-message) (error error-message)) t)
    response))

(defun llm-vertex--chat (provider prompt response-callback error-callback sync)
  "Get the chat response for PROMPT.
PROVIDER, RESPONSE-CALLBACK, ERROR-CALLBACK are all the same as
`llm-chat-async'. SYNC, when non-nil, will wait until
the response is available to return."
  (llm-vertex-refresh-key provider)
  (let ((request-alist))
    (when (llm-chat-prompt-context prompt)
      (push `("context" . ,(llm-chat-prompt-context prompt)) request-alist))
    (when (llm-chat-prompt-examples prompt)
      (push `("examples" . ,(apply #'vector
                                   (mapcar (lambda (example)
                                      `(("input" . (("content" . ,(car example))))
                                        ("output" . (("content" . ,(cdr example))))))
                                           (llm-chat-prompt-examples prompt))))
            request-alist))
    (push `("messages" . ,(apply #'vector
                                 (mapcar (lambda (interaction)
                                           `(("author" . (pcase (llm-chat-prompt-interaction-role interaction)
                                                           ('user "user")
                                                           ('system (error "System role not supported"))
                                                           ('assistant "assistant")))
                                             ("content" . ,(llm-chat-prompt-interaction-content interaction))))
                                         (llm-chat-prompt-interactions prompt))))
          request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt))
            request-alist))
    (when (llm-chat-prompt-max-tokens prompt)
      (push `("max_tokens" . ,(llm-chat-prompt-max-tokens prompt)) request-alist))
    (request (format "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:predict"
                                   llm-vertex-gcloud-region
                                   (llm-vertex-project provider)
                                   llm-vertex-gcloud-region
                                   (or (llm-vertex-chat-model provider) "chat-bison"))
      :type "POST"
      :sync sync
      :headers `(("Authorization" . ,(format "Bearer %s" (llm-vertex-key provider)))
                 ("Content-Type" . "application/json"))
      :data (json-encode `(("instances" . [,request-alist])))
      :parser 'json-read
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (funcall response-callback
                                       (cdr (assoc 'content (aref (cdr (assoc 'candidates (aref (cdr (assoc 'predictions data)) 0))) 0))))))
      :error (cl-function (lambda (&key error-thrown data &allow-other-keys)
                            (funcall error-callback 'error
                                     (error (format "Problem calling GCloud AI: %s, status: %s message: %s (%s)"
                                                    (cdr error-thrown)
                                                    (assoc-default 'status (assoc-default 'error data))
                                                    (assoc-default 'message (assoc-default 'error data))
                                                    data))))))))

(cl-defmethod llm-chat-async ((provider llm-vertex) prompt response-callback error-callback)
  (llm-vertex--chat provider prompt response-callback error-callback nil))

(cl-defmethod llm-chat ((provider llm-vertex) prompt)
  (let ((response))
    (llm-vertex--chat provider prompt
                               (lambda (result) (setq response result))
                               (lambda (_ error-message) (error error-message)) t)
    response))

(provide 'llm-vertex)

;;; llm-vertex.el ends here
