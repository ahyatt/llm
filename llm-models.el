;;; llm-models.el --- Specification of model capabilities -*- lexical-binding: t; package-lint-main-file: "llm.el" -*-

;; Copyright (c) 2024  Free Software Foundation, Inc.

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
;; This file specifies the capabilities of the models that can be used
;; by the `llm' package.

;;; Code:
(require 'cl-lib)
(require 'rx)
(require 'seq)

(cl-defstruct llm-model
  "A struct representing a model.
NAME is the name of the model, appropriate for showing a user.

CAPABILITIES is a list of symbols representing the capabilities of the
model, one of `embedding', `generation', `tool-use',
`image-input', `image-output', `audio-input', `video-input', `caching'
and `free-software'.

REGEX is a regular expression that can be used to identify the model, uniquely (it shouldn't conflict with any other model)"
  name
  symbol
  capabilities
  context-length
  regex)

(defconst llm-models
  (list
   ;; https://platform.openai.com/docs/models
   (make-llm-model
    :name "GPT-3.5 Turbo" :symbol 'gpt-3.5-turbo
    :capabilities '(generation tool-use)
    :context-length 16385
    :regex "gpt-3\\.5-turbo\\'")
   (make-llm-model
    :name "GPT-3.5 Turbo Instruct" :symbol 'gpt-3.5-turbo-instruct
    :capabilities '(generation tool-use)
    :context-length 4096
    :regex "gpt-3\\.5-turbo-instruct")
   (make-llm-model
    :name "GPT-4o" :symbol 'gpt-4o
    :capabilities '(generation tool-use image-input)
    :context-length 128000
    :regex "gpt-4o\\'")
   (make-llm-model
    :name "GPT-4o mini" :symbol 'gpt-4o-mini
    :capabilities '(generation tool-use image-input)
    :context-length 128000
    :regex "gpt-4o-mini")
   (make-llm-model
    :name "o1 Preview" :symbol 'o1-preview
    :capabilities '(generation)
    :context-length 128000
    :regex "o1-preview")
   (make-llm-model
    :name "o1 Mini" :symbol 'o1-mini
    :capabilities '(generation)
    :context-length 128000
    :regex "o1-mini")
   (make-llm-model
    :name "GPT-4 Turbo" :symbol 'gpt-4-turbo
    :capabilities '(generation tool-use image-input)
    :context-length 128000
    :regex (rx (or "gpt-4-turbo" "gpt-4-0125" "gpt-4-1106")))
   (make-llm-model
    :name "GPT-4" :symbol 'gpt-4
    :capabilities '(generation tool-use image-input)
    :context-length 8192
    :regex (rx (or (seq "gpt-4" string-end) "gpt-4-0613" "gpt-4-0314")))
   (make-llm-model
    :name "text-embedding-3-large" :symbol 'text-embedding-3-large
    :capabilities '(embedding)
    :context-length 8192
    :regex "text-embedding-3-large")
   (make-llm-model
    :name "text-embedding-3-small" :symbol 'text-embedding-3-small
    :capabilities '(embedding)
    :context-length 8192
    :regex "text-embedding-3-small")
   (make-llm-model
    :name "text-embedding-ada-002" :symbol 'text-embedding-ada-002
    :capabilities '(embedding)
    :context-length 8192
    :regex "text-embedding-ada-002")
   ;; https://docs.anthropic.com/en/docs/about-claude/models
   (make-llm-model
    :name "Claude 3.5 Sonnet" :symbol 'claude-3.5-sonnet
    :capabilities '(generation tool-use image-input caching)
    :context-length 200000
    :regex "claude-3.5-sonnet")
   (make-llm-model
    :name "Claude 3 Opus" :symbol 'claude-3-opus
    :capabilities '(generation tool-use image-input caching)
    :context-length 200000
    :regex "claude-3-opus")
   (make-llm-model
    :name "Claude 3 Sonnet" :symbol 'claude-3-sonnet
    :capabilities '(generation tool-use image-input caching)
    :context-length 200000
    :regex "claude-3-sonnet")
   (make-llm-model
    :name "Claude 3 Haiku" :symbol 'claude-3-haiku
    :capabilities '(generation tool-use image-input)
    :context-length 200000
    :regex "claude-3-haiku")
   ;; https://ai.google.dev/gemini-api/docs/models/gemini
   (make-llm-model
    :name "Gemini 1.5 Flash" :symbol 'gemini-1.5-flash
    :capabilities '(generation tool-use image-input audio-input video-input)
    :context-length 1048576
    :regex "gemini-1\\.5-flash")
   (make-llm-model
    :name "Gemini 1.5 Pro" :symbol 'gemini-1.5-pro
    :capabilities '(generation tool-use image-input audio-input video-input)
    :context-length 2097152
    :regex "gemini-1\\.5-pro")
   (make-llm-model
    :name "Gemini 1.0 Pro" :symbol 'gemini-1.0-pro
    :capabilities '(generation tool-use)
    ;; Context length is not specified in the documentation
    :context-length 8192
    :regex (rx (or "gemini-1\\.0-pro" "gemini-pro")))
   (make-llm-model
    :name "Text Embedding (Gemini)" :symbol 'gemini-text-embedding-004
    :capabilities '(embedding)
    :context-length 2048
    :regex "text-embedding-004")
   (make-llm-model
    :name "Embedding (Gemini)" :symbol 'gemini-embedding-001
    :capabilities '(embedding)
    :context-length 2048
    :regex "embedding-001")
   ;; https://ollama.com/library?sort=popular
   (make-llm-model
    :name "Llama 3" :symbol 'llama-3
    :capabilities '(generation)
    :context-length 8192
    :regex "llama-?3\\'")
   (make-llm-model
    :name "Llama 3.1" :symbol 'llama-3.1
    :capabilities '(generation tool-use)
    :context-length 128000
    :regex "llama-?3\\.1")
   (make-llm-model
    :name "Llama 3.2" :symbol 'llama-3.2
    :capabilities '(generation tool-use)
    :context-length 128000
    :regex "llama-?3\\.2")
   (make-llm-model
    :name "Gemma 2" :symbol 'gemma-2
    :capabilities '(generation free-software)  ;; Apache license
    :context-length 8192
    :regex "gemma-?2")
   (make-llm-model
    :name "Mistral" :symbol 'mistral
    :capabilities '(generation tool-use free-software)  ;; Apache license
    :context-length 8192
    :regex "mistral")
   (make-llm-model
    :name "Llava" :symbol 'llava
    :capabilities '(generation image-input free-software)  ;; Apache license
    :context-length 4096
    :regex "llava")
   (make-llm-model
    :name "Nomic" :symbol 'nomic-embed-text
    :capabilities '(embedding free-software)  ;; Apache license
    :context-length 8192
    :regex "nomic-embed-text")
   (make-llm-model
    :name "MXBai Embed Large" :symbol 'mxbai-embed-large
    :capabilities '(embedding free-software)  ;; Apache license
    :context-length 512
    :regex "mxbai-embed-large")
   (make-llm-model
    :name "All MiniLM" :symbol 'all-minilm
    :capabilities '(embedding free-software)  ;; Apache license
    :context-length 256
    :regex "all-minilm")
   (make-llm-model
    :name "Snowflake Artic Embed" :symbol 'snowflake-artic-embed
    :capabilities '(embedding free-software)  ;; Apache license
    :context-length 8192
    :regex "snowflake-artic-embed")
   (make-llm-model
    :name "Qwen 2.5" :symbol 'qwen-2.5
    :capabilities '(generation tool-use)  ;; Apache license for some variations only
    :context-length 128000
    :regex "qwen-2\\.5")
   (make-llm-model
    :name "Nemotron Mini" :symbol 'nemotron-mini
    :capabilities '(generation tool-use)
    :context-length 4096
    :regex "nemotron-mini")))

(defun llm-models-by-symbol (symbol)
  "Return the model with SYMBOL."
  (cl-find symbol llm-models :key #'llm-model-symbol))

(defun llm-models-match (name)
  "Return the model that matches NAME."
  (seq-find (lambda (model) (string-match-p (llm-model-regex model) (downcase name))) llm-models))

(provide 'llm-models)

;;; llm-models.el ends here
