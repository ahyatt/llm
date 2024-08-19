;;; llm-gpt4all.el --- llm module for integrating with GPT4All -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
;; This files implements the llm functionality defined in llm.el, for GPT4All.
;; The GPT4All API is based on Open AI's, so we depend on the llm-openai module
;; here.
;;
;; GPT4All does not support embeddings.
;;
;; Users using GPT4All must enable the API Server in their GPT4All settings for
;; this to work.

;;; Code:
(require 'llm)
(require 'llm-openai)
(require 'llm-provider-utils)

(cl-defstruct (llm-gpt4all (:include llm-openai-compatible))
  "A structure for holding information needed by GPT4All.

CHAT-MODEL is the model to use for chat queries. It must be set.

URL is the host to connect to.  If unset, it will default to http://localhost.

PORT is the port to connect to (an integer). If unset, it will
default the default GPT4all port."
  host port)

(cl-defmethod llm-provider-chat-url ((provider llm-gpt4all))
  "Return the URL for PATH, given the settings in PROVIDER."
  (format "http://%s:%d/v1/chat/completions" (or (llm-gpt4all-host provider) "localhost")
          (or (llm-gpt4all-port provider) 4891)))

(cl-defmethod llm-chat-streaming ((provider llm-gpt4all) prompt _partial-callback response-callback error-callback)
  ;; GPT4All does not implement streaming, so instead we just use the async method.
  (llm-chat-async provider prompt response-callback error-callback))

(cl-defmethod llm-name ((provider llm-gpt4all))
  "Return the name of the PROVIDER."
  (llm-gpt4all-chat-model provider))

(cl-defmethod llm-chat-token-limit ((provider llm-gpt4all))
  (llm-provider-utils-model-token-limit (llm-gpt4all-chat-model provider)))

(provide 'llm-gpt4all)

;;; llm-gpt4all.el ends here
