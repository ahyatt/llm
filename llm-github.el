;;; llm-github.el --- llm module for integrating with GitHub Models -*- lexical-binding: t; package-lint-main-file: "llm.el"; byte-compile-docstring-max-column: 200 -*-

;; Copyright (c) 2024 Free Software Foundation, Inc.

;; Author: Gabriel Santos de Souza <gabrielsantosdesouza@disroot.org>
;; Homepage: https://github.com/ahyatt/llm
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This file implements the llm functionality defined in llm.el,
;; for the GitHub Models platform.

;;; Code:

(require 'llm)
(require 'llm-azure)

(cl-defstruct (llm-github (:include llm-azure
                                    (url "https://models.inference.ai.azure.com"))))

(cl-defmethod llm-provider-chat-url ((provider llm-github))
  (format "%s/chat/completions" (llm-azure-url provider)))

(cl-defmethod llm-name ((provider llm-github))
  (format "GitHub Models %s" (llm-github-chat-model provider)))

(provide 'llm-github)
;;; llm-github.el ends here
