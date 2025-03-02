;;; llm-gemini.el --- LLM implementation of Google Cloud Gemini AI -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
;; This file implements the llm functionality defined in llm.el, for Google's
;; Gemini AI.  he documentation is at
;; https://ai.google.dev/tutorials/rest_quickstart.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-vertex)
(require 'llm-provider-utils)
(require 'json)

(cl-defstruct (llm-gemini (:include llm-google))
  "A struct representing a Gemini client.

KEY is the API key for the client.
You can get this at https://makersuite.google.com/app/apikey."
  key (embedding-model "embedding-001") (chat-model "gemini-2.0-flash-001"))

(cl-defmethod llm-nonfree-message-info ((_ llm-gemini))
  "Return nonfree terms of service for Gemini."
  "https://policies.google.com/terms/generative-ai")

(cl-defmethod llm-provider-embedding-url ((provider llm-gemini) &optional _)
  "Return the URL for the EMBEDDING request for STRING from PROVIDER."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:embedContent?key=%s"
          (llm-gemini-embedding-model provider)
          (if (functionp (llm-gemini-key provider))
              (funcall (llm-gemini-key provider))
            (llm-gemini-key provider))))

(cl-defmethod llm-provider-embedding-request ((provider llm-gemini) string)
  `(:model ,(llm-gemini-embedding-model provider)
           :content (:parts [(:text ,string)])))

(cl-defmethod llm-provider-embedding-extract-result ((_ llm-gemini) response)
  (assoc-default 'values (assoc-default 'embedding response)))

;; from https://ai.google.dev/tutorials/rest_quickstart
(defun llm-gemini--chat-url (provider streaming-p)
  "Return the URL for the chat request, using PROVIDER.
If STREAMING-P is non-nil, use the streaming endpoint."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:%s?key=%s"
          (llm-gemini-chat-model provider)
          (if streaming-p "streamGenerateContent" "generateContent")
          (if (functionp (llm-gemini-key provider))
              (funcall (llm-gemini-key provider))
            (llm-gemini-key provider))))

(cl-defmethod llm-provider-chat-url ((provider llm-gemini))
  (llm-gemini--chat-url provider nil))

(cl-defmethod llm-provider-chat-streaming-url ((provider llm-gemini))
  (llm-gemini--chat-url provider t))

(cl-defmethod llm-provider-chat-request ((_ llm-gemini) _ _)
  ;; Temporary, can be removed in the next version.  Without this the old
  ;; definition will cause problems when users upgrade.
  (cl-call-next-method))

(cl-defmethod llm-name ((_ llm-gemini))
  "Return the name of PROVIDER."
  "Gemini")

(cl-defmethod llm-chat-token-limit ((provider llm-gemini))
  (llm-provider-utils-model-token-limit (llm-gemini-chat-model provider)
                                        1048576))

(cl-defmethod llm-capabilities ((provider llm-gemini))
  (append
   (list 'streaming 'embeddings)
   (when-let ((model (llm-models-match (llm-gemini-chat-model provider)))
              (capabilities (llm-model-capabilities model)))
     (append
      (when (member 'tool-use capabilities) '(tool-use streaming-tool-use))
      (seq-intersection capabilities '(image-input audio-input video-input))))))

(provide 'llm-gemini)

;;; llm-gemini.el ends here
