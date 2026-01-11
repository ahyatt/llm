;;; llm-claude.el --- llm module for integrating with Claude -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
;; This file implements the llm functionality defined in llm.el, for Claude's
;; API.

;;; Code:

(require 'llm)
(require 'llm-provider-utils)
(require 'plz-event-source)
(require 'plz)
(require 'rx)

;; Models defined at https://docs.anthropic.com/claude/docs/models-overview
(cl-defstruct (llm-claude (:include llm-standard-chat-provider))
  (key nil :read-only t)
  (chat-model "claude-sonnet-4-5" :read-only t))

(cl-defmethod llm-nonfree-message-info ((_ llm-claude))
  "Return Claude's nonfree ToS."
  "https://www.anthropic.com/legal/consumer-terms")

(cl-defmethod llm-provider-prelude ((provider llm-claude))
  (unless (llm-claude-key provider)
    (signal 'llm-provider-unconfigured '("No API key provided for Claude"))))

(defun llm-claude--tool-call (tool)
  "A Claude version of a function spec for TOOL."
  `(:name ,(llm-tool-name tool)
          :description ,(llm-tool-description tool)
          :input_schema ,(llm-provider-utils-openai-arguments
                          (llm-tool-args tool))))

(cl-defmethod llm-provider-chat-request ((provider llm-claude) prompt stream)
  (let ((request
          `(:model ,(llm-claude-chat-model provider)
                   :stream ,(if stream t :false)
                   ;; Claude requires max_tokens
                   :max_tokens ,(or (llm-chat-prompt-max-tokens prompt)
                                    (cond ((string-match "opus-4-0" (llm-claude-chat-model provider))
                                           32000)
                                          ((or (string-match "4" (llm-claude-chat-model provider))
                                               (string-match "3-7" (llm-claude-chat-model provider)))
                                           64000)
                                          ((string-match "opus" (llm-claude-chat-model provider))
                                           4096)
                                          (t
                                           8192)))
                   :messages
                   ,(vconcat
                     (mapcar (lambda (interaction)
                               `(:role  ,(pcase (llm-chat-prompt-interaction-role interaction)
                                           ('tool_results "user")
                                           ('tool_use "assistant")
                                           ('assistant "assistant")
                                           ('user "user"))
                                        :content
                                        ,(cond ((llm-chat-prompt-interaction-tool-results interaction)
                                                (vconcat (mapcar (lambda (result)
                                                                   `(:type "tool_result"
                                                                           :tool_use_id
                                                                           ,(llm-chat-prompt-tool-result-call-id result)
                                                                           :content
                                                                           ,(llm-chat-prompt-tool-result-result result)))
                                                                 (llm-chat-prompt-interaction-tool-results interaction))))
                                               ((llm-multipart-p (llm-chat-prompt-interaction-content interaction))
                                                (llm-claude--multipart-content
                                                 (llm-chat-prompt-interaction-content interaction)))
                                               (t
                                                (llm-chat-prompt-interaction-content interaction)))))
                             (llm-chat-prompt-interactions prompt)))))
        (system (llm-provider-utils-get-system-prompt prompt)))
    (when (llm-chat-prompt-tools prompt)
      (setq request (plist-put request :tools
                               (vconcat
                                (mapcar (lambda (f) (llm-claude--tool-call f))
                                        (llm-chat-prompt-tools prompt))))))
    (when (and (llm-chat-prompt-response-format prompt)
               (member 'json-response (llm-capabilities provider))
               (not (eq 'json (llm-chat-prompt-response-format prompt))))
      (setq request (plist-put request :output_format
                               `(:type "json_schema"
                                       :schema ,(let ((schema (llm-chat-prompt-response-format prompt)))
                                                  (unless (plist-get schema :additionalProperties)
                                                    (setq schema (plist-put schema :additionalProperties :false)))
                                                  (llm-provider-utils-convert-to-serializable
                                                   schema))))))
    (when (> (length system) 0)
      (setq request (plist-put request :system system)))
    (when (llm-chat-prompt-temperature prompt)
      (setq request (plist-put request :temperature (llm-chat-prompt-temperature prompt))))
    (when-let* ((options (llm-chat-prompt-tool-options prompt)))
      (setq request (plist-put request :tool_choice
                               (append
                                (list :type (pcase (llm-tool-options-tool-choice options)
                                              ('auto "auto")
                                              ('any "any")
                                              ('none "none")
                                              ((pred stringp) "tool")
                                              (_ (signal 'llm-not-supported
                                                         (list (format "Unknown tool choice option: %s"
                                                                       (llm-tool-options-tool-choice options)))))))
                                (when (stringp (llm-tool-options-tool-choice options))
                                  (list :name (llm-tool-options-tool-choice options)))))))
    (when (llm-chat-prompt-reasoning prompt)
      (setq request (plist-put request :thinking
                               (let (thinking-plist)
                                 (setq thinking-plist (plist-put thinking-plist
                                                                 :type
                                                                 (if (eq (llm-chat-prompt-reasoning prompt) 'none)
                                                                     "disabled" "enabled")))
                                 (if (not (eq (llm-chat-prompt-reasoning prompt) 'none))
                                     (plist-put thinking-plist :budget_tokens
                                                (pcase (llm-chat-prompt-reasoning prompt)
                                                  ('light 3000)
                                                  ('medium 10000)
                                                  ('maximum 32000))))
                                 thinking-plist))))
    (append request (llm-provider-utils-non-standard-params-plist prompt))))

(defun llm-claude--multipart-content (content)
  "Return CONTENT as a list of Claude multipart content."
  (vconcat
   (mapcar (lambda (part)
             (cond ((stringp part)
                    `(:type "text"
                            :text ,part))
                   ((llm-media-p part)
                    (let ((source (list :type "base64"
                                        :media_type (llm-media-mime-type part)
                                        :data (base64-encode-string (llm-media-data part) t))))
                      `(:type ,(if (equal (llm-media-mime-type part) "application/pdf")
                                   "document"
                                 "image")
                              :source ,source)))
                   (t
                    (signal 'llm-invalid-argument
                            (list (format "Unsupported multipart content: %s" part))))))
           (llm-multipart-parts content))))
(cl-defmethod llm-provider-extract-tool-uses ((_ llm-claude) response)
  (let ((content (append (assoc-default 'content response) nil)))
    (cl-loop for item in content
             when (equal "tool_use" (assoc-default 'type item))
             collect (make-llm-provider-utils-tool-use
                      :id (assoc-default 'id item)
                      :name (assoc-default 'name item)
                      :args (assoc-default 'input item)))))

(cl-defmethod llm-provider-populate-tool-uses ((_ llm-claude) prompt tool-uses)
  (llm-provider-utils-append-to-prompt
   prompt
   (vconcat (mapcar (lambda (call)
                      `(:type "tool_use"
                              :id ,(llm-provider-utils-tool-use-id call)
                              :name ,(llm-provider-utils-tool-use-name call)
                              :input ,(llm-provider-utils-tool-use-args call)))
                    tool-uses))))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-claude) response)
  (when (> (length (assoc-default 'content response)) 0)
    (let ((content (aref (assoc-default 'content response) 0)))
      (assoc-default 'text content))))

(cl-defmethod llm-provider-extract-reasoning ((_ llm-claude) response)
  (when (> (length (assoc-default 'content response)) 0)
    (let ((content (aref (assoc-default 'content response) 0)))
      (assoc-default 'thinking content))))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-claude)
                                                    receiver err-receiver)
  (cons 'text/event-stream
        (plz-event-source:text/event-stream
         :events `((message_start . ignore)
                   (ping . ignore)
                   (message_stop . ignore)
                   (content_block_stop . ignore)
                   (message_delta . ignore)
                   (error . ,(lambda (event)
                               (funcall err-receiver (plz-event-source-event-data event))))
                   (content_block_start
                    .
                    ,(lambda (event)
                       (let* ((data (plz-event-source-event-data event))
                              (json (json-parse-string data :object-type 'alist))
                              (block (assoc-default 'content_block json))
                              (type (assoc-default 'type block))
                              (index (assoc-default 'index json)))
                         (when (equal type "tool_use")
                           (let ((id (assoc-default 'id block))
                                 (name (assoc-default 'name block)))
                             (funcall receiver `(:tool-uses-raw
                                                 ,(vector
                                                   (list
                                                    'id id
                                                    'name name
                                                    'index index
                                                    'input "")))))))))
                   (content_block_delta
                    .
                    ,(lambda (event)
                       (let* ((data (plz-event-source-event-data event))
                              (json (json-parse-string data :object-type 'alist))
                              (delta (assoc-default 'delta json))
                              (type (assoc-default 'type delta))
                              (index (assoc-default 'index json)))
                         (cond
                          ((equal type "text_delta")
                           (funcall receiver `(:text ,(assoc-default 'text delta))))
                          ((equal type "thinking_delta")
                           (funcall receiver `(:reasoning ,(assoc-default 'text delta))))
                          ((equal type "input_json_delta")
                           (funcall receiver `(:tool-uses-raw
                                               ,(vector
                                                 (list
                                                  'input (assoc-default 'partial_json delta)
                                                  'index index)))))))))))))

(cl-defmethod llm-provider-collect-streaming-tool-uses ((_ llm-claude) data)
  "Transform Claude streaming tool-uses DATA responses into tool use structs.
DATA is a vector of lists produced by `llm-provider-streaming-media-handler'."
  (let ((tools (make-hash-table :test 'equal))
        (index-to-id (make-hash-table :test 'eql))
        result)
    (cl-loop for entry across data do
             (if (plist-get entry 'id)
                 ;; new tool use
                 (let ((id (plist-get entry 'id))
                       (name (plist-get entry 'name))
                       (index (plist-get entry 'index)))
                   (puthash id (make-llm-provider-utils-tool-use
                                :id id :name name :args "")
                            tools)
                   (puthash index id index-to-id))
               ;; tool input update
               (let* ((index (plist-get entry 'index))
                      (input (plist-get entry 'input))
                      (id (gethash index index-to-id))
                      (tool (gethash id tools)))
                 (setf (llm-provider-utils-tool-use-args tool)
                       (concat (llm-provider-utils-tool-use-args tool) input)))))
    (maphash (lambda (_ tool)
               (condition-case nil
                   (let ((args (llm-provider-utils-tool-use-args tool)))
                     (setf (llm-provider-utils-tool-use-args tool)
                           (if (string-empty-p args)
                               nil
                             (json-parse-string args :object-type 'alist))))
                 (error nil))
               (push tool result))
             tools)
    (nreverse result)))

(cl-defmethod llm-provider-headers ((provider llm-claude))
  (append
   `(("x-api-key" . ,(if (functionp (llm-claude-key provider))
                         (funcall (llm-claude-key provider))
                       (llm-claude-key provider)))
     ("anthropic-version" . "2023-06-01"))
   (when (member 'json-response (llm-capabilities provider))
     '(("anthropic-beta" . "structured-outputs-2025-11-13")))))

(cl-defmethod llm-provider-chat-extract-error ((_ llm-claude) response)
  (when-let ((err (assoc-default 'error response)))
    (format "Error %s: '%s'" (assoc-default 'type err)
            (assoc-default 'message err))))

(defun llm-claude--url (method)
  "Return a Claude API URL for METHOD."
  (format "https://api.anthropic.com/v1/%s" method))

(cl-defmethod llm-provider-chat-url ((_ llm-claude))
  "Return the URL for the Claude API."
  (llm-claude--url "messages"))

(cl-defmethod llm-chat-token-limit ((provider llm-claude))
  (llm-provider-utils-model-token-limit (llm-claude-chat-model provider)))

(cl-defmethod llm-name ((_ llm-claude))
  "Return the name of the provider."
  "Claude")

(cl-defmethod llm-capabilities ((provider llm-claude))
  (seq-union
   '(streaming tool-use streaming-tool-use image-input pdf-input reasoning)
   (when-let* ((model (llm-models-match (llm-claude-chat-model provider))))
     (llm-model-capabilities model))))

(cl-defmethod llm-provider-append-to-prompt ((_ llm-claude) prompt result
                                             &optional tool-use-results)
  ;; Claude doesn't have a 'function role, so we just always use assistant here.
  ;; But if it's a function result, it considers that a 'user response, which
  ;; needs to be sent back.
  (llm-provider-utils-append-to-prompt prompt result tool-use-results (if tool-use-results
                                                                          'user
                                                                        'assistant)))

(cl-defmethod llm-models ((provider llm-claude))
  (mapcar (lambda (model)
            (plist-get model :id))
          (append
           (plist-get (plz 'get (llm-claude--url "models")
                        :as (lambda () (json-parse-buffer :object-type 'plist))
                        :headers (llm-provider-headers provider))
                      :data)
           nil)))

(provide 'llm-claude)

;;; llm-claude.el ends here
