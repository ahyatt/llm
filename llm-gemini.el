;;; llm-gemini.el --- LLM implementation of Google Cloud Gemini AI -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
;; This file implements the llm functionality defined in llm.el, for Google's
;; Gemini AI.  The documentation is at
;; https://ai.google.dev/tutorials/rest_quickstart.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-vertex)
(require 'llm-provider-utils)
(require 'llm-typesafe)
(require 'json)

(defconst llm-gemini--base-url "https://generativelanguage.googleapis.com/v1beta")

(cl-defstruct (llm-gemini-decide
               (:include llm-typesafe-compatible)
               (:constructor make-llm-gemini-decide
                             (&key (model "gemini-decision-1")
                                   ((:key raw-key))
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key))
                                   (url (concat llm-gemini--base-url "/decisions")))))
  "A Gemini Decision model provider.")

(cl-defstruct (llm-gemini
               (:include llm-google)
               (:constructor make-llm-gemini
                             (&key default-chat-temperature
                                   default-chat-max-tokens
                                   default-chat-non-standard-params
                                   ((:key raw-key))
                                   (embedding-model "gemini-embedding-001")
                                   (chat-model "gemini-3.8-flash")
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key))
                                   (decider (make-llm-gemini-decide
                                             :key raw-key)))))
  "A struct representing a Gemini client.

KEY is the API key for the client.
You can get this at https://makersuite.google.com/app/apikey."
  key embedding-model chat-model decider)

(defun llm-gemini--auth-headers (key)
  `(("x-goog-api-key" . ,(if (functionp key)
                             (funcall key)
                           key))))

(cl-defmethod llm-nonfree-message-info ((_ llm-gemini))
  "Return nonfree terms of service for Gemini."
  "https://policies.google.com/terms/generative-ai")

(cl-defmethod llm-provider-embedding-url ((provider llm-gemini) &optional _)
  "Return the URL for the EMBEDDING request for STRING from PROVIDER."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:embedContent"
          (llm-gemini-embedding-model provider)))

(cl-defmethod llm-provider-headers ((provider llm-gemini))
  (llm-gemini--auth-headers (llm-gemini-key provider)))

(cl-defmethod llm-provider-embedding-request ((provider llm-gemini) string)
  `(:model ,(llm-gemini-embedding-model provider)
           :content (:parts [(:text ,string)])))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-gemini) response)
  (assoc-default 'values (assoc-default 'embedding response)))

;; from https://ai.google.dev/tutorials/rest_quickstart
(defun llm-gemini--chat-url (provider streaming-p)
  "Return the URL for the chat request, using PROVIDER.
If STREAMING-P is non-nil, use the streaming endpoint."
  (format "%s/models/%s:%s"
          llm-gemini--base-url
          (llm-gemini-chat-model provider)
          (if streaming-p "streamGenerateContent" "generateContent")))

(cl-defmethod llm-provider-chat-url ((provider llm-gemini))
  (llm-gemini--chat-url provider nil))

(cl-defmethod llm-provider-chat-streaming-url ((provider llm-gemini))
  (llm-gemini--chat-url provider t))

(cl-defmethod llm-provider-chat-request ((provider llm-gemini) prompt _)
  (llm-provider--chat-request prompt (let ((model (llm-models-match (llm-gemini-chat-model provider))))
                                       (if model
                                           (llm-model-symbol model)
                                         'unknown))))

(cl-defmethod llm-name ((_ llm-gemini))
  "Return the name of PROVIDER."
  "Gemini")

(cl-defmethod llm-chat-token-limit ((provider llm-gemini))
  (llm-provider-utils-model-token-limit (llm-gemini-chat-model provider)
                                        1048576))

;;; Decision model code; Gemini is mostly but not quite compatible with
;;; Typesafe's AI.

(cl-defmethod llm-decide ((provider llm-gemini) questions state)
  ;; Synchronize the key, which may have been changed.
  (setf (llm-gemini-decide-key (llm-gemini-decider provider))
        (llm-gemini-key provider))
  (llm-decide (llm-gemini-decider provider) questions state))

(cl-defmethod llm-decide-async ((provider llm-gemini) questions state result-callback error-callback)
  ;; Synchronize the key, which may have been changed.
  (setf (llm-gemini-decide-key (llm-gemini-decider provider))
        (llm-gemini-key provider))
  (llm-decide-async (llm-gemini-decider provider) questions state result-callback error-callback))

(cl-defmethod llm-nonfree-message-info ((_ llm-gemini-decide))
  "Return Gemini's nonfree terms of service."
  "https://policies.google.com/terms/generative-ai")

(cl-defmethod llm-provider-request-prelude ((provider llm-gemini-decide))
  (unless (llm-gemini-decide-key provider)
    (signal 'llm-provider-unconfigured
            '("To call Gemini API, add a key to the `llm-gemini' provider"))))

(cl-defmethod llm-provider-headers ((provider llm-gemini-decide))
  (llm-gemini--auth-headers (llm-gemini-decide-key provider)))

(cl-defmethod llm-provider-decide-url ((provider llm-gemini-decide))
  (llm-gemini-decide-url provider))

(cl-defmethod llm-provider-headers ((provider llm-gemini-decide))
  (llm-gemini--auth-headers (llm-gemini-decide-key provider)))

(cl-defmethod llm-provider-decide-extract-error ((_ llm-gemini-decide) response)
  (llm-google--extract-error response))

(cl-defmethod llm-provider-decide-request ((provider llm-gemini-decide) questions state)
  (list
   :model (llm-typesafe-compatible-model provider)
   :questions (mapcar (lambda (q)
                        (if (llm-question-bool-p q)
                            (llm-gemini--bool-question-request q)
                          (llm-typesafe-question-request q)))
                      questions)
   :input state))

(defun llm-gemini--bool-question-request (question)
  (cons (llm-question-bool-name question)
        (append
         (list :type "noul"
               :instructions (llm-question-bool-instructions question))
         (when (or (llm-question-bool-true-description question)
                   (llm-question-bool-false-description question))
           (list :criteria (append
                            (when (llm-question-bool-true-description question)
                              (list :true_description (llm-question-bool-true-description question)))
                            (when (llm-question-bool-false-description question)
                              (list :false_description (llm-question-bool-false-description question)))))))))

(cl-defmethod llm-capabilities ((provider llm-gemini))
  (append
   '(streaming embeddings model-list decision)
   (when-let* ((model (llm-models-match (llm-gemini-chat-model provider)))
               (capabilities (llm-model-capabilities model)))
     (seq-uniq (append
                (when (member 'tool-use capabilities) '(tool-use streaming-tool-use))
                capabilities
                (seq-intersection capabilities '(image-input audio-input video-input)))))))

(cl-defmethod llm-models ((provider llm-gemini))
  (mapcar (lambda (model)
            (plist-get model :name))
          (append
           (plist-get (plz 'get (format "%s/models/" llm-gemini--base-url)
                        :headers (llm-gemini--auth-headers (llm-gemini-key provider))
                        :as (lambda () (json-parse-buffer :object-type 'plist)))
                      :models)
           nil)))

(provide 'llm-gemini)

;;; llm-gemini.el ends here
