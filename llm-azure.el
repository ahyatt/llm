;;; llm-azure.el --- llm module for integrating with Azure's Open AI service -*- lexical-binding: t; package-lint-main-file: "llm.el"; byte-compile-docstring-max-column: 200-*-

;; Copyright (c) 2024-2025  Free Software Foundation, Inc.

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
;; This file implements the llm functionality defined in llm.el, for Azure's
;; Open AI service.

;;; Code:

(require 'llm)
(require 'llm-openai)
(require 'cl-lib)

(cl-defstruct (llm-azure (:include llm-openai-compatible)))

(cl-defmethod llm-nonfree-message-info ((_ llm-azure))
  "Return Azure's nonfree terms of service."
  "https://azure.microsoft.com/en-us/support/legal/")

(cl-defmethod llm-provider-chat-url ((provider llm-azure))
  (format "%s/openai/deployments/%s/chat/completions?api-version=2024-08-01-preview"
          (llm-azure-url provider)
          (llm-azure-chat-model provider)))

(cl-defmethod llm-provider-embedding-url ((provider llm-azure) &optional _)
  (format "%s/openai/deployments/%s/embeddings?api-version=2024-08-01-preview"
          (llm-azure-url provider)
          (llm-azure-embedding-model provider)))

(cl-defmethod llm-provider-headers ((provider llm-azure))
  `(("api-key" . ,(if (functionp (llm-azure-key provider))
                      (funcall (llm-azure-key provider))
                    (llm-azure-key provider)))))

(cl-defmethod llm-capabilities ((_ llm-azure))
  (list 'streaming 'embedding))

(cl-defmethod llm-name ((provider llm-azure))
  (format "Azure OpenAI %s" (llm-azure-chat-model provider)))

(provide 'llm-azure)
;;; llm-azure.el ends here
