;;; llm-llamacpp.el --- llm module for integrating with llama.cpp. -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023, 2024  Free Software Foundation, Inc.

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
;; This file implements the llm functionality defined in llm.el, for llama.cpp,
;; which can be found at https://github.com/ggerganov/llama.cpp.

;;; Code:

(require 'llm)
(require 'cl-lib)
(require 'llm-openai)
(require 'llm-provider-utils)
(require 'json)

(defgroup llm-llamacpp nil
  "LLM implementation for llama.cpp."
  :group 'llm)

(defcustom llm-llamacpp-example-prelude "Example of how you should response follow."
  "The prelude to use for examples."
  :type 'string
  :group 'llm-llamacpp)

(defcustom llm-llamacpp-history-prelude "You are in the middle of a conversation between you and a user.  First, we will give you the previous conversation between you ('assistant') and the user, so you have the context, and then give you the latest message for you to response to.  The previous conversation follows."
  "The prelude to use when there has been more than one interaction already.
This is needed because there is no API support for previous chat conversation."
  :type 'string)

;; Obsolete, llm-openai-compatible can be used directly instead.
(cl-defstruct (llm-llamacpp (:include llm-openai-compatible))
  "A struct representing a llama.cpp instance."
  (scheme "http") (host "localhost") (port 8080))

(defun llm-llamacpp--url (provider path)
  "From PROVIDER, return the URL for llama.cpp.
PATH is the path to append to the URL, not prefixed with a slash."
  (lwarn 'llm-llamacpp :warning
         "The LlamaCPP module is deprecated, you should use the Open AI Compatible provider instead")
  (let ((scheme (llm-llamacpp-scheme provider))
        (host (llm-llamacpp-host provider))
        (port (llm-llamacpp-port provider)))
    (format "%s://%s:%d/%s" scheme host port path)))

(cl-defmethod llm-provider-embedding-url ((provider llm-llamacpp))
  (llm-llamacpp--url provider "embedding"))

(cl-defmethod llm-provider-chat-url ((provider llm-llamacpp))
  (llm-llamacpp--url provider "chat/completions"))

(cl-defmethod llm-name ((_ llm-llamacpp))
  "Name of Llama CPP, because we don't know the model name."
  "Llama CPP")

(cl-defmethod llm-capabilities ((_ llm-llamacpp))
  "Return the capabilities of llama.cpp."
  (list 'streaming 'embeddings))

(provide 'llm-llamacpp)
;;; llm-llamacpp.el ends here
