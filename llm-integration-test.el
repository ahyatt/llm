;;; llm-intgration-test.el --- Integration tests for the llm module -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2024-2025  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
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
;; This tests the `llm' module by running against real backends. It is designed
;; to be as fast and accurate as possible, but since LLMs are not deterministic,
;; some flakiness may happen.
;;
;; These tests will test multiple models, according to the environment variables
;; set:
;;
;; - OPENAI_KEY: An OpenAI API key.
;; - ANTHROPIC_KEY: An Anthropic API key, for Claude.
;; - GEMINI_KEY: A Gemini API key.
;; - VERTEX_PROJECT: A Google Cloud Vertex project.
;; - OLLAMA_CHAT_MODELS: A list of Ollama models to test.
;; - DEEPSEEK_KEY: A DeepSeek API key.
;; - AZURE_URL: The URL of the Azure API.
;; - AZURE_KEY: The key for the Azure API.
;; - AZURE_CHAT_MODEL: The name of the chat model to test.
;; - AZURE_EMBEDDING_MODEL: The name of the embedding model to test.
;; - AZURE_SLEEP: The number of seconds to sleep between tests, to avoid rate
;;   limiting.
;; - GITHUB_TOKEN: The key for GitHub models. Can either be a GitHub token or
;;   an Azure production key.
;;
;; If any of these are set (except for Azure, which needs multiple), the
;; corresponding provider will be tested.


;;; Code:

(require 'llm)
(require 'ert)
(require 'seq)
(require 'image)
(require 'llm-test)

(defconst llm-integration-test-chat-prompt
  "What is the capital of France?  Give me only one word, in English, with no punctuation."
  "A chat prompt to use for testing.")

(defconst llm-integration-test-chat-answer
  "Paris"
  "The correct answer to the chat prompt.")

(defconst llm-integration-current-directory
  (file-truename
   (file-name-directory (locate-dominating-file (or load-file-name default-directory)
                                                "llm.el")))
  "The directory of this file.")

(defun llm-integration-test-tool-use-prompt ()
  "Return a function call prompt for testing."
  (llm-make-chat-prompt
   "What is the capital of France?"
   :tools
   (list (llm-make-tool
          :function (lambda (callback result) (funcall callback result))
          :name "capital_of_country"
          :description "Get the capital of a country."
          :args '((:name "country"
                         :description "The country whose capital to look up."
                         :type string))
          :async t))))

(defconst llm-integration-test-fc-answer
  '(("capital_of_country" . "France"))
  "The correct answer to the function call prompt.")

(defun llm-integration-test-fc-multiple-prompt ()
  (llm-make-chat-prompt
   "What is the capital of France, and also what is the capital of Italy?"
   :tools
   (list (llm-make-tool
          :function (lambda (f) f)
          :name "capital_of_country"
          :description "Get the capital of a country."
          :args '((:name "country"
                         :description "The country whose capital to look up."
                         :type string))
          :async nil))))

(defun llm-integration-test-rate-limit (provider)
  (cond ((eq (type-of provider) 'llm-azure)
         ;; The free Azure tier has extremely restrictive rate limiting.
         (sleep-for (string-to-number (or (getenv "AZURE_SLEEP") "60"))))
        ((member (type-of provider) '(llm-gemini llm-vertex))
         (sleep-for 30))))

(defun llm-integration-test-string-eq (target actual)
  "Test that TARGET approximately equals ACTUAL.
This is a very approximate test because LLMs that aren't that great
often mess up and put punctuation, or repeat the word, or something
else.  We really just want to see if it's in the right ballpark."
  (string-match-p (regexp-quote (downcase target)) (downcase actual)))

(defun llm-integration-test-providers ()
  "Return a list of providers to test."
  (let ((providers))
    (when (getenv "OPENAI_KEY")
      (require 'llm-openai)
      (push (make-llm-openai :key (getenv "OPENAI_KEY")) providers))
    (when (getenv "ANTHROPIC_KEY")
      (require 'llm-claude)
      (push (make-llm-claude :key (getenv "ANTHROPIC_KEY")) providers))
    (when (getenv "GEMINI_KEY")
      (require 'llm-gemini)
      (push (make-llm-gemini :key (getenv "GEMINI_KEY")) providers))
    (when (getenv "VERTEX_PROJECT")
      (require 'llm-vertex)
      (push (make-llm-vertex :project (getenv "VERTEX_PROJECT")) providers))
    (when (and (getenv "AZURE_URL") (getenv "AZURE_KEY"))
      (require 'llm-azure)
      ;; Because Azure requires setting up each model, we can't just use any
      ;; model it supports, but the model that the user running tests has set up.
      (push (make-llm-azure :key (getenv "AZURE_KEY") :url (getenv "AZURE_URL")
                            :chat-model (getenv "AZURE_CHAT_MODEL")
                            :embedding-model (getenv "AZURE_EMBEDDING_MODEL"))
            providers))
    (when (getenv "GITHUB_TOKEN")
      (require 'llm-github)
      (push (make-llm-github :key (getenv "GITHUB_TOKEN")) providers))
    (when (getenv "DEEPSEEK_KEY")
      (require 'llm-deepseek)
      (push (make-llm-deepseek :key (getenv "DEEPSEEK_KEY")) providers)
      (push (make-llm-deepseek :key (getenv "DEEPSEEK_KEY") :chat-model "deepseek-reasoner") providers))
    (when (getenv "OLLAMA_CHAT_MODELS")
      (require 'llm-ollama)
      ;; This variable is a list of models to test.
      (dolist (model (split-string (getenv "OLLAMA_CHAT_MODELS") ", "))
        (push (make-llm-ollama :chat-model model) providers)))
    providers))

(defmacro llm-def-integration-test (name arglist &rest body)
  "Define an integration test."
  (declare (indent defun))
  `(ert-deftest ,name ()
     (let ((llm-warn-on-nonfree nil))
       (dolist (,(car arglist) (llm-integration-test-providers))
         (llm-integration-test-rate-limit provider)
         (ert-info ((format "Using provider %s" (llm-name provider)))
           ,@body)))))

(llm-def-integration-test llm-embedding (provider)
  (when (member 'embeddings (llm-capabilities provider))
    (let ((result (llm-embedding provider "Paris")))
      (should (vectorp result))
      (should (> (length result) 0)))))

(llm-def-integration-test llm-embedding-async (provider)
  (when (member 'embeddings (llm-capabilities provider))
    (let ((result nil)
          (buf (current-buffer))
          (llm-warn-on-nonfree nil))
      (llm-embedding-async
       provider
       "Paris"
       (lambda (response)
         (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
         (setq result response))
       (lambda (error)
         (error "Error: %s" error)))
      (while (null result)
        (sleep-for 0.1))
      (should (vectorp result))
      (should (> (length result) 0)))))

(llm-def-integration-test llm-batch-embeddings (provider)
  (when (member 'embeddings-batch (llm-capabilities provider))
    (let ((result (llm-batch-embeddings provider '("Paris" "France"))))
      (should (listp result))
      (should (= (length result) 2))
      (should (vectorp (aref result 0)))
      (should (vectorp (aref result 1))))))

(llm-def-integration-test llm-batch-embedding-async (provider)
  (when (member 'embeddings-batch (llm-capabilities provider))
    (let ((result nil)
          (buf (current-buffer))
          (llm-warn-on-nonfree nil))
      (llm-batch-embeddings-async
       provider
       '("Paris" "France")
       (lambda (response)
         (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
         (setq result response))
       (lambda (error)
         (error "Error: %s" error)))
      (while (null result)
        (sleep-for 0.1))
      (should (listp result))
      (should (= (length result) 2))
      (should (vectorp (aref result 0)))
      (should (vectorp (aref result 1))))))

(llm-def-integration-test llm-chat (provider)
  (should (equal
           (string-trim (llm-chat
                         provider
                         (llm-make-chat-prompt llm-integration-test-chat-prompt)))
           llm-integration-test-chat-answer)))

(llm-def-integration-test llm-chat-multi-output (provider)
  (should (equal
           (string-trim (plist-get
                         (llm-chat
                          provider
                          (llm-make-chat-prompt llm-integration-test-chat-prompt)
                          t) :text))
           llm-integration-test-chat-answer)))

(llm-def-integration-test llm-chat-async (provider)
  (let ((result nil)
        (buf (current-buffer))
        (llm-warn-on-nonfree nil)
        (err-result nil))
    (llm-chat-async
     provider
     (llm-make-chat-prompt llm-integration-test-chat-prompt)
     (lambda (response)
       (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
       (setq result response))
     (lambda (_ err)
       (setq err-result err)))
    (while (not (or result err-result))
      (sleep-for 0.1))
    (if err-result (error err-result))
    (should (llm-integration-test-string-eq llm-integration-test-chat-answer (string-trim result)))))

(llm-def-integration-test llm-chat-async-multi-output (provider)
  (let ((result nil)
        (buf (current-buffer))
        (llm-warn-on-nonfree nil)
        (err-result nil))
    (llm-chat-async
     provider
     (llm-make-chat-prompt llm-integration-test-chat-prompt)
     (lambda (response)
       (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
       (setq result response))
     (lambda (_ err)
       (setq err-result err))
     t)
    (while (not (or result err-result))
      (sleep-for 0.1))
    (if err-result (error err-result))
    (should (plist-get result :text))
    (should (llm-integration-test-string-eq llm-integration-test-chat-answer (string-trim (plist-get result :text))))))

(llm-def-integration-test llm-chat-streaming (provider)
  (when (member 'streaming (llm-capabilities provider))
    (let ((streamed-result "")
          (returned-result nil)
          (llm-warn-on-nonfree nil)
          (buf (current-buffer))
          (start-time (current-time))
          (err-result nil))
      (llm-chat-streaming
       provider
       (llm-make-chat-prompt llm-integration-test-chat-prompt)
       (lambda (partial-response)
         (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
         (setq streamed-result (concat streamed-result partial-response)))
       (lambda (response)
         (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
         (setq returned-result response))
       (lambda (_ err)
         (setq err-result err)))
      (while (and (or (null returned-result)
                      (= (length streamed-result) 0))
                  (null err-result)
                  (time-less-p (time-subtract (current-time) start-time) 60))
        (sleep-for 0.1))
      (if err-result (error err-result))
      (should (llm-integration-test-string-eq llm-integration-test-chat-answer (string-trim returned-result)))
      (should (llm-integration-test-string-eq llm-integration-test-chat-answer (string-trim streamed-result))))))

(llm-def-integration-test llm-chat-streaming-multi-output (provider)
  (when (member 'streaming (llm-capabilities provider))
    (let ((streamed-result "")
          (returned-result nil)
          (llm-warn-on-nonfree nil)
          (buf (current-buffer))
          (start-time (current-time))
          (err-result nil))
      (llm-chat-streaming
       provider
       (llm-make-chat-prompt llm-integration-test-chat-prompt)
       (lambda (partial-response)
         (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
         (setq streamed-result partial-response))
       (lambda (response)
         (should (or (not (buffer-live-p buf)) (eq (current-buffer) buf)))
         (setq returned-result response))
       (lambda (_ err)
         (setq err-result err))
       t)
      (while (and (or (null returned-result)
                      (= (length streamed-result) 0))
                  (null err-result)
                  (time-less-p (time-subtract (current-time) start-time) 60))
        (sleep-for 0.1))
      (if err-result (error err-result))
      (should (llm-integration-test-string-eq llm-integration-test-chat-answer (string-trim (plist-get returned-result :text))))
      (should (llm-integration-test-string-eq llm-integration-test-chat-answer (string-trim (plist-get streamed-result :text)))))))

(llm-def-integration-test llm-tool-use (provider)
  (when (member 'tool-use (llm-capabilities provider))
    (let ((prompt (llm-integration-test-tool-use-prompt)))
      (should (equal
               (llm-chat provider prompt)
               llm-integration-test-fc-answer))
      ;; Test that we can send the function back to the provider without error.
      (llm-chat provider prompt))))

(llm-def-integration-test llm-tool-use-multi-output (provider)
  (when (member 'tool-use (llm-capabilities provider))
    (let* ((prompt (llm-integration-test-tool-use-prompt))
           (result (llm-chat provider prompt t)))
      (should (equal
               (plist-get result :tool-results)
               llm-integration-test-fc-answer))
      (should (plist-get result :tool-uses))
      (if (plist-get result :text)
          (should (> (length (plist-get result :text)) 0)))
      ;; Test that we can send the function back to the provider without error.
      (llm-chat provider prompt t))))

(llm-def-integration-test llm-tool-use-streaming-multi-output (provider)
  (when (member 'streaming-tool-use (llm-capabilities provider))
    (let* ((prompt (llm-integration-test-tool-use-prompt))
           (result nil))
      (llm-chat-streaming provider prompt #'ignore (lambda (response) (setq result response)) (lambda (_ err) (error err)) t)
      (while (null result)
        (sleep-for 0.1))
      (should (equal
               (plist-get result :tool-results)
               llm-integration-test-fc-answer))
      (should (plist-get result :tool-uses))
      (if (plist-get result :text)
          (should (> (length (plist-get result :text)) 0))))))

(llm-def-integration-test llm-tool-use-multiple (provider)
  (when (member 'tool-use (llm-capabilities provider))
    (let ((prompt (llm-integration-test-fc-multiple-prompt)))
      ;; Sending back multiple answers often doesn't happen, so we can't reliably
      ;; check for this yet.
      (llm-chat provider prompt)
      ;; Test that we can send the function back to the provider without error.
      (llm-chat provider prompt))))

(llm-def-integration-test llm-image-chat (provider)
  ;; On github, the emacs we use doesn't have image support, so we can't use
  ;; image objects.
  (when (member 'image-input (llm-capabilities provider))
    (let* ((image-bytes
            (with-temp-buffer (set-buffer-multibyte nil)
                              (insert-file-contents-literally
                               (expand-file-name "animal.jpeg" llm-integration-current-directory))
                              (buffer-string)))
           (result (llm-chat
                    provider
                    (llm-make-chat-prompt
                     (llm-make-multipart
                      "What is this animal?  You should have an image, if not please let me know.  If you do have the image, please answer in one word, without punctuation or whitespace."
                      (make-llm-media :mime-type "image/jpeg" :data image-bytes))))))
      (should (stringp result))
      (should (llm-integration-test-string-eq "owl" (string-trim (downcase result)))))))

(llm-def-integration-test
  llm-pdf-chat (provider)
  (when (member 'pdf-input (llm-capabilities provider))
    (let* ((pdf-data
            (with-temp-buffer (set-buffer-multibyte nil)
                              (insert-file-contents-literally
                               (expand-file-name "test.pdf" llm-integration-current-directory))
                              (buffer-string)))
           (result (llm-chat
                    provider
                    (llm-make-chat-prompt
                     (llm-make-multipart
                      "What symbol occurs in the PDF file?  If you do not see a PDF file, please let me know.  If you do, please answer in one letter, without punctuation or whitespace."
                      (make-llm-media :mime-type "application/pdf" :data pdf-data))))))
      (should (stringp result))
      (should (llm-integration-test-string-eq "x" (string-trim (downcase result)))))))

(llm-def-integration-test llm-json-test (provider)
  (when (member 'json-response (llm-capabilities provider))
    (let ((result (llm-chat
                   provider
                   (llm-make-chat-prompt
                    "List the 3 largest cities in France in order of population, giving the results in JSON."
                    :response-format
                    '(:type "object"
                            :properties
                            (:cities (:type array :items (:type string)))
                            :required ["cities"])))))
      (should (equal
               (llm-test-normalize '(:cities ["Lyon" "Marseille" "Paris"]))
               (llm-test-normalize (json-parse-string result :object-type 'plist)))))))

(llm-def-integration-test llm-count-tokens (provider)
  (let ((result (llm-count-tokens provider "What is the capital of France?")))
    (should (integerp result))
    (should (> result 0))))

(provide 'llm-integration-test)
