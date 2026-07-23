;;; llm-models-test.el --- Test of model capabilities spec -*- lexical-binding: t; package-lint-main-file: "llm.el" -*-

;; Copyright (c) 2024-2025  Free Software Foundation, Inc.

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
;; This file contains tests for llm-models.el.

;;; Code:

(require 'llm-models)
(require 'ert)

(ert-deftest llm-models-match-unique ()
  ;; It shouldn't be the case that things match because they are first.  So we
  ;; will test various model names against the models and reverse the order of
  ;; the models to ensure that the first match is the unique match.
  (let ((models-names-to-test '("llama3" "gemini-3.5-flash"
                                "gemma3" "qwen-3.7" "kimi-k3"
                                "gpt-5.5" "claude-opus-4-8"
                                "claude-fable-5" "deepseek-r1")))
    (dolist (model-name models-names-to-test)
      (let ((model-forward (llm-models-match model-name))
            (model-reverse (let ((llm-models (reverse llm-models)))
                             (llm-models-match model-name))))
        (ert-info ((format "Testing model %s" model-name))
          (should (eq model-forward model-reverse))
          ;; Also, we expect to actually find these
          (should model-forward))))))
