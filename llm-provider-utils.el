;;; llm-provider-utils.el --- Functions to make building providers easier -*- lexical-binding: t; package-lint-main-file: "llm.el"; ; byte-compile-docstring-max-column: 200-*-

;; Copyright (c) 2023-2025  Free Software Foundation, Inc.

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
;; This file provides functions to help build providers.  It should only be used
;; by modules implementing an LLM provider.

;;; Code:

(require 'llm)
(require 'llm-request-plz)
(require 'llm-models)
(require 'seq)
(require 'compat)

(cl-defstruct llm-standard-provider
  "A struct indicating that this is a standard provider.
This is for dispatch purposes, so this contains no actual data.

This represents any provider, regardless of what it implements.

This should not be used outside of this file.")

(cl-defstruct (llm-standard-chat-provider (:include llm-standard-provider))
  "A struct for indicating a provider that implements chat.
DEFAULT-CHAT-TEMPERATURE is the default temperature for chats
with the provider.  Any `temperature' specified in the chat
prompt will override this.  This is optional, and if not set,
when not overridden, the default value chosen by the provider
will be used.

DEFAULT-CHAT-MAX-TOKENS is the default maxmimum number of tokens
for chats with the provider.  Any value for `max-tokens'
specified in the chat prompt will override this.  This is
optional, and if not set, when not overriden, no maximum will be
specified to the provider.

DEFAULT-CHAT-NON-STANDARD-PARAMS are per-provider params that
will override and `non-standard-params' that are part of the
prompt.  This is an alist of parameters, whose name and possible
values will be different for each provider.  The overriding here
is on a per-parameter basis, so the final value used in the chat
can be a mix of these default parameters and others in the
prompt.

These values will be set as parameters on the prompt, so changing
values after the initial call in the chat will not have an
effect.  New values will have an effect, however."
  default-chat-temperature default-chat-max-tokens
  default-chat-non-standard-params)

(cl-defstruct (llm-standard-full-provider (:include llm-standard-chat-provider))
  "A struct for providers that implements chat and embeddings.")

(cl-defstruct llm-provider-utils-tool-use
  "A struct to hold information about a tool use.
ID is a call ID, which is optional.
NAME is the tool name.
ARG is an alist of arguments to their values."
  id name args)

;; Methods necessary for both embedding and chat requests.

(cl-defgeneric llm-provider-request-prelude (provider)
  "Execute any prelude code necessary before running a request.
PROVIDER is the provider that will be used to make the request.")

(cl-defmethod llm-provider-request-prelude ((_ llm-standard-provider))
  "Do nothing for the standard provider."
  nil)

(cl-defgeneric llm-provider-headers (provider)
  "Return the headers for the PROVIDER.")

(cl-defmethod llm-provider-headers ((_ llm-standard-provider))
  "By default, the standard provider has no headers."
  nil)

;; Methods for embeddings
(cl-defgeneric llm-provider-embedding-url (provider &optional batch)
  "Return the URL for embeddings for the PROVIDER.
BATCH is true if this is a batch request.")

(cl-defgeneric llm-provider-embedding-request (provider string)
  "Return the request for the PROVIDER for STRING.")

(cl-defgeneric llm-provider-batch-embeddings-request (provider string-list)
  "Return the request for the PROVIDER for STRING-LIST.")

(cl-defgeneric llm-provider-embedding-extract-error (provider response)
  "Return an error message from RESPONSE for the PROVIDER.

RESPONSE is a parsed JSON object.

Return nil if there is no error.")

(cl-defmethod llm-provider-embedding-extract-error ((_ llm-standard-full-provider) _)
  "By default, the standard provider has no error extractor."
  nil)

(cl-defgeneric llm-provider-embedding-extract-result (provider response)
  "Return the result from RESPONSE for the PROVIDER.")

(cl-defgeneric llm-provider-batch-embeddings-extract-result (provider response)
  "Return the result from RESPONSE for the PROVIDER for a batch request.")

;; Methods for chat

(cl-defgeneric llm-provider-chat-url (provider)
  "Return the URL for chat for the PROVIDER.")

(cl-defgeneric llm-provider-chat-streaming-url (provider)
  "Return the URL for streaming chat for the PROVIDER.")

(cl-defmethod llm-provider-chat-streaming-url ((provider llm-standard-chat-provider))
  "By default, use the same URL as normal chat.

PROVIDER is the standard chat provider that is used to make the
request."
  (llm-provider-chat-url provider))

(cl-defgeneric llm-provider-chat-timeout (provider)
  "Return the seconds of timeout for PROVIDER.
Return nil for the standard timeout.")

(cl-defmethod llm-provider-chat-timeout ((_ llm-standard-provider))
  "By default, the standard provider has the standard timeout."
  nil)

(cl-defmethod llm-provider-chat-request :before ((provider llm-standard-chat-provider) prompt _)
  "Set PROVIDER default parameters where they do not existe in the PROMPT."
  (setf (llm-chat-prompt-temperature prompt)
        (or (llm-chat-prompt-temperature prompt)
            (llm-standard-chat-provider-default-chat-temperature provider))
        (llm-chat-prompt-max-tokens prompt)
        (or (llm-chat-prompt-max-tokens prompt)
            (llm-standard-chat-provider-default-chat-max-tokens provider))
        (llm-chat-prompt-non-standard-params prompt)
        ;; We need to merge the parameters individually.
        ;; Lists as values should be turned into vectors.
        (mapcar (lambda (c)
                  (if (listp (cdr c))
                      (cons (car c) (vconcat (cdr c)))
                    c))
                (seq-union (llm-chat-prompt-non-standard-params prompt)
                           (llm-standard-chat-provider-default-chat-non-standard-params provider)
                           (lambda (a b)
                             (equal (car a) (car b)))))))

(cl-defgeneric llm-provider-chat-request (provider prompt streaming)
  "Return the request for the PROVIDER for PROMPT.
STREAMING is true if this is a streaming request.")

(cl-defgeneric llm-provider-chat-extract-error (provider response)
  "Return an error message from RESPONSE for the PROVIDER.")

(cl-defmethod llm-provider-chat-extract-error ((_ llm-standard-chat-provider) _)
  "By default, the standard provider has no error extractor."
  nil)

(cl-defgeneric llm-provider-chat-extract-result (provider response)
  "Return the result from RESPONSE for the PROVIDER.")

(cl-defgeneric llm-provider-append-to-prompt (provider prompt result &optional tool-results)
  "Append RESULT to PROMPT for the PROVIDER.

PROMPT is the prompt that was already sent to the provider.

TOOL-RESULTS is a list of function results, if any.")

(cl-defmethod llm-provider-append-to-prompt ((_ llm-standard-chat-provider) prompt
                                             result &optional tool-results)
  ;; By default, we just append to the prompt.
  (llm-provider-utils-append-to-prompt prompt result tool-results))

(cl-defgeneric llm-provider-streaming-media-handler (provider receiver err-receiver)
  "Define how to handle streaming media for the PROVIDER.

This should return a cons of the media type and an instance that
handle objects of that type.

The handlers defined can call RECEIVER with a plist compatible with the
output of the llm functions returned when `multi-output' is set.  If
they receive an error, they should call ERR-RECEIVER with the error
message.")

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-standard-chat-provider) _ _)
  "By default, the standard provider has no streaming media handler."
  nil)

;; Methods for chat function calling

(cl-defgeneric llm-provider-extract-tool-uses (provider response)
  "Return the tool-uses from RESPONSE for the PROVIDER.

If there are no tool uses, return nil.  If there are tool uses, return a
list of `llm-provider-utils-tool-use'.")

(cl-defmethod llm-provider-extract-tool-uses ((_ llm-standard-chat-provider) _)
  "By default, the standard provider has no function call extractor."
  nil)

(cl-defgeneric llm-provider-extract-reasoning (provider response)
  "Return the reasoning from RESPONSE for the PROVIDER.")

(cl-defmethod llm-provider-extract-reasoning ((_ llm-standard-chat-provider) _)
  "By default, the standard provider has no reasoning extractor."
  nil)

(cl-defgeneric llm-provider-populate-tool-uses (provider prompt tool-uses)
  "For PROVIDER, in PROMPT, record TOOL-USES.
This is the recording before the function calls were executed, in the prompt.
CALLS are a list of `llm-provider-utils-tool-use'.")

(cl-defgeneric llm-provider-collect-streaming-tool-uses (provider data)
  "Transform a list of streaming tool-uses DATA responses.

PROVIDER is the struct specifying the LLM provider and its configuration.

The DATA responses are a list of whatever is sent to the tool
use handler in `llm-provider-streaming-media-handler'.  This should
return a list of `llm-chat-prompt-tool-use' structs.")

(cl-defmethod llm-provider-collect-streaming-tool-uses ((_ llm-standard-chat-provider) _)
  ;; by default, there is no function calling
  nil)

;; Standard provider implementations of llm functionality

(cl-defmethod llm-embedding ((provider llm-standard-full-provider) string)
  (llm-provider-request-prelude provider)
  (let ((response (llm-request-plz-sync
                   (llm-provider-embedding-url provider nil)
                   :timeout (llm-provider-chat-timeout provider)
                   :headers (llm-provider-headers provider)
                   :data (llm-provider-embedding-request provider string))))
    (if-let ((err-msg (llm-provider-embedding-extract-error provider response)))
        (error err-msg)
      (llm-provider-embedding-extract-result provider response))))

(cl-defmethod llm-embedding-async ((provider llm-standard-full-provider) string vector-callback error-callback)
  (llm-provider-request-prelude provider)
  (let ((buf (current-buffer)))
    (llm-request-plz-async
     (llm-provider-embedding-url provider nil)
     :headers (llm-provider-headers provider)
     :data (llm-provider-embedding-request provider string)
     :on-success (lambda (data)
                   (if-let ((err-msg (llm-provider-embedding-extract-error provider data)))
                       (llm-provider-utils-callback-in-buffer
                        buf error-callback 'error
                        err-msg)
                     (llm-provider-utils-callback-in-buffer
                      buf vector-callback
                      (llm-provider-embedding-extract-result provider data))))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-embedding-extract-error
                         provider data)
                        "Unknown error")))))))

(cl-defmethod llm-batch-embeddings ((provider llm-standard-full-provider) string-list)
  (llm-provider-request-prelude provider)
  (let ((response (llm-request-plz-sync
                   (llm-provider-embedding-url provider t)
                   :timeout (llm-provider-chat-timeout provider)
                   :headers (llm-provider-headers provider)
                   :data (llm-provider-batch-embeddings-request provider string-list))))
    (if-let ((err-msg (llm-provider-embedding-extract-error provider response)))
        (error err-msg)
      (llm-provider-batch-embeddings-extract-result provider response))))

(cl-defmethod llm-batch-embeddings-async ((provider llm-standard-full-provider) string-list vector-callback error-callback)
  (llm-provider-request-prelude provider)
  (let ((buf (current-buffer)))
    (llm-request-plz-async
     (llm-provider-embedding-url provider t)
     :headers (llm-provider-headers provider)
     :data (llm-provider-batch-embeddings-request provider string-list)
     :on-success (lambda (data)
                   (if-let ((err-msg (llm-provider-embedding-extract-error provider data)))
                       (llm-provider-utils-callback-in-buffer
                        buf error-callback 'error
                        err-msg)
                     (llm-provider-utils-callback-in-buffer
                      buf vector-callback
                      (llm-provider-batch-embeddings-extract-result provider data))))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-embedding-extract-error
                         provider data)
                        "Unknown error")))))))

(defun llm-provider-utils-extract-all (provider response)
  "Extract all from RESPONSE for the PROVIDER."
  (let ((text
         (llm-provider-chat-extract-result provider response))
        (tool-uses (llm-provider-extract-tool-uses
                    provider response))
        (reasoning (llm-provider-extract-reasoning
                    provider response)))
    (append (when text `(:text ,text))
            (when tool-uses `(:tool-uses ,tool-uses))
            (when reasoning `(:reasoning ,reasoning)))))

(cl-defmethod llm-chat ((provider llm-standard-chat-provider) prompt &optional multi-output)
  (llm-provider-request-prelude provider)
  (let ((response (llm-request-plz-sync (llm-provider-chat-url provider)
                                        :headers (llm-provider-headers provider)
                                        :data (llm-provider-chat-request provider prompt nil)))
        (final-result nil))
    (if-let ((err-msg (llm-provider-chat-extract-error provider response)))
        (error err-msg)
      (llm-provider-utils-process-result provider prompt
                                         (llm-provider-utils-extract-all
                                          provider response)
                                         multi-output
                                         (lambda (result)
                                           (setq final-result result))))
    ;; In most cases, final-result will be available immediately.  However, when
    ;; executing tools, we need to wait for their callbacks, and only after
    ;; those are called with this be ready.
    (while (not final-result)
      (sleep-for 0.1))
    final-result))

(cl-defmethod llm-chat-async ((provider llm-standard-chat-provider) prompt success-callback
                              error-callback &optional multi-output)
  (llm-provider-request-prelude provider)
  (let ((buf (current-buffer)))
    (llm-request-plz-async
     (llm-provider-chat-url provider)
     :headers (llm-provider-headers provider)
     :data (llm-provider-chat-request provider prompt nil)
     :on-success (lambda (data)
                   (if-let ((err-msg (llm-provider-chat-extract-error provider data)))
                       (llm-provider-utils-callback-in-buffer
                        buf error-callback 'error
                        err-msg)
                     (llm-provider-utils-process-result
                      provider prompt
                      (llm-provider-utils-extract-all provider data)
                      multi-output
                      (lambda (result)
                        (llm-provider-utils-callback-in-buffer
                         buf success-callback result)))))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-chat-extract-error
                         provider data)
                        "Unknown error")))))))

(defun llm-provider-utils-streaming-accumulate (current new)
  "Add streaming NEW to CURRENT and return the result.

This is designed to accumulate responses for streaming results.  It
assumes that CURRENT and NEW are the same type of thing..

This will work with text as well as the plists that are returned when
`multi-output' is on.

Any strings will be concatenated, integers will be added, etc."
  (if current
      (if new
          (progn
            (unless (eq (type-of current) (type-of new))
              (error "Cannot accumulate different types of streaming results: %s and %s"
                     current new))
            (pcase (type-of current)
              ('string (concat current new))
              ('integer (+ current new))
              ('float (+ current new))
              ('vector (vconcat current new))
              ('cons (if (and (> (length current) 0)  ;; if plist
                              (symbolp (car current))
                              (string-match-p "^:" (symbol-name (car current))))
                         (cl-loop for key in
                                  (seq-union (map-keys current)
                                             (map-keys new))
                                  append
                                  (list key
                                        (llm-provider-utils-streaming-accumulate
                                         (plist-get current key)
                                         (plist-get new key))))
                       (append current new)))))
        current)
    new))

(cl-defmethod llm-chat-streaming ((provider llm-standard-chat-provider) prompt partial-callback
                                  response-callback error-callback &optional multi-output)
  (llm-provider-request-prelude provider)
  (let ((buf (current-buffer))
        (current-result))
    (llm-request-plz-async
     (llm-provider-chat-streaming-url provider)
     :headers (llm-provider-headers provider)
     :data (llm-provider-chat-request provider prompt t)
     :media-type (llm-provider-streaming-media-handler
                  provider
                  (lambda (s)
                    (setq current-result
                          (llm-provider-utils-streaming-accumulate current-result s))
                    (when partial-callback
                      (when-let* ((callback-val (if multi-output
                                                    current-result
                                                  (plist-get current-result :text))))
                        (llm-provider-utils-callback-in-buffer
                         buf partial-callback callback-val))))
                  (lambda (err)
                    (llm-provider-utils-callback-in-buffer
                     buf error-callback 'error
                     err)))
     :on-success
     (lambda (_)
       ;; We don't need the data at the end of streaming, so we can ignore it.
       (llm-provider-utils-process-result
        provider prompt
        (llm-provider-utils-streaming-accumulate
         current-result
         (when-let ((tool-uses-raw (plist-get current-result
                                              :tool-uses-raw)))
           `(:tool-uses ,(llm-provider-collect-streaming-tool-uses
                          provider tool-uses-raw))))
        multi-output
        (lambda (result)
          (llm-provider-utils-callback-in-buffer
           buf response-callback result))))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-chat-extract-error
                         provider data)
                        "Unknown error")))))))

(defun llm-provider-utils-get-system-prompt (prompt &optional example-prelude)
  "From PROMPT, turn the context and examples into a string.
EXAMPLE-PRELUDE is a string to prepend to the examples."
  (concat
   (llm-chat-prompt-context prompt)
   (when (llm-chat-prompt-context prompt) "\n")
   (when (llm-chat-prompt-examples prompt) (or example-prelude
                                               (concat
                                                (if (= (length (llm-chat-prompt-examples prompt)) 1)
                                                    "Here is an example"
                                                  (format "Here are %d examples"
                                                          (length (llm-chat-prompt-examples prompt))))
                                                " of how to respond:\n")))
   (when (llm-chat-prompt-examples prompt) "\n")
   (mapconcat (lambda (example)
                (format "User: %s\nAssistant: %s"
                        (car example)
                        (cdr example)))
              (llm-chat-prompt-examples prompt) "\n")))

(defun llm-provider-utils-combine-to-system-prompt (prompt &optional example-prelude)
  "Add context and examples to a system prompt in PROMPT.

This should be used for providers that have a notion of a system prompt.
If there is a system prompt, and no assistant response, add to it.
If there is no system prompt, create one.
If there is an assistance response, do nothing.

EXAMPLE-PRELUDE is the text to introduce any examples with."
  (let ((system-prompt (seq-find
                        (lambda (interaction)
                          (eq (llm-chat-prompt-interaction-role interaction) 'system))
                        (llm-chat-prompt-interactions prompt)))
        (system-content (llm-provider-utils-get-system-prompt prompt example-prelude)))
    (when (and system-content (> (length system-content) 0))
      (if system-prompt
          (setf (llm-chat-prompt-interaction-content system-prompt)
                (concat (llm-chat-prompt-interaction-content system-prompt)
                        "\n"
                        system-content))
        (push (make-llm-chat-prompt-interaction
               :role 'system
               :content system-content)
              (llm-chat-prompt-interactions prompt))
        (setf (llm-chat-prompt-context prompt) nil
              (llm-chat-prompt-examples prompt) nil)))))

(defun llm-provider-utils-combine-to-user-prompt (prompt &optional example-prelude)
  "Add context and examples to a user prompt in PROMPT.
This should be used for providers that do not have a notion of a system prompt.

EXAMPLE-PRELUDE is the text to introduce any examples with."
  (let ((system-content (llm-provider-utils-get-system-prompt prompt example-prelude)))
    (when (> (length system-content) 0)
      (setf (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt)))
            (let ((initial-content (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt)))))
              (if (llm-multipart-p initial-content)
                  (make-llm-multipart
                   :parts (cons system-content
                                (llm-multipart-parts initial-content)))
                (concat system-content
                        "\n"
                        initial-content)))
            (llm-chat-prompt-context prompt) nil
            (llm-chat-prompt-examples prompt) nil))))

(defun llm-provider-utils-collapse-history (prompt &optional history-prelude)
  "Collapse history to a single PROMPT.

This is useful for providers that cannot handle conversations.
Essentially it's a way to fake conversation.  aution: tokens will
eventually run out, though, so this isn't a sustainable way to do
things.  Providers should probably issue a warning when using this.

HISTORY-PRELUDE is the text to use to tell the LLM that
conversation history will follow."
  (when (> (length (llm-chat-prompt-interactions prompt)) 1)
    (setf (llm-chat-prompt-interactions prompt)
          (list (make-llm-chat-prompt-interaction
                 :role 'user
                 :content
                 (concat (or history-prelude "Previous interactions:") "\n\n"
                         (mapconcat (lambda (interaction)
                                      (format "%s: %s" (pcase (llm-chat-prompt-interaction-role interaction)
                                                         ('user "User")
                                                         ('assistant "Assistant"))
                                              (llm-chat-prompt-interaction-content interaction)))
                                    (butlast (llm-chat-prompt-interactions prompt)) "\n")
                         "\n\nThe current conversation follows:\n\n"
                         (llm-chat-prompt-interaction-content (car (last (llm-chat-prompt-interactions prompt))))))))))

(defun llm-provider-utils-model-token-limit (model &optional default)
  "Return the token limit for MODEL.
If MODEL cannot be found, warn and return DEFAULT, which by default is 4096."
  (let ((matched-model (llm-models-match model)))
    (if matched-model
        (llm-model-context-length matched-model)
      (warn "No model predefined for model %s, using restrictive defaults" model)
      (or default 4096))))

(defun llm-provider-utils--encolon (s)
  "Turn S into a symbol preceded by a colon."
  (intern (format ":%s" s)))

(defun llm-provider-utils-non-standard-params-plist (prompt)
  "Return non-standard-paramters from PROMPT as a plist."
  (mapcan (lambda (pcons) (list (llm-provider-utils--encolon (car pcons))
                                (cdr pcons)))
          (llm-chat-prompt-non-standard-params prompt)))

(defun llm-provider-utils--decolon (sym)
  "Remove a colon from the beginnging of SYM."
  (let ((s (symbol-name sym)))
    (if (string-prefix-p ":" s)
        (intern (substring s 1))
      sym)))

(defun llm-provider-utils-convert-to-serializable (plist)
  "Convert PLIST to a serializable form.

The expectation is that any symbol values will be converted to strings
for plist and any nested plists."
  (mapcan (lambda (elem-pair)
            (cond ((member (nth 1 elem-pair) '(:json-false :false))
                   (list (car elem-pair) :false))
                  ((eq (nth 1 elem-pair) t)
                   (list (car elem-pair) t))
                  ((not (nth 1 elem-pair))
                   (list (car elem-pair) :null))
                  ((symbolp (nth 1 elem-pair))
                   (list (car elem-pair)
                         (symbol-name (nth 1 elem-pair))))
                  ((consp (nth 1 elem-pair))
                   (list (car elem-pair)
                         (llm-provider-utils-convert-to-serializable (nth 1 elem-pair))))
                  (t elem-pair)))
          (seq-partition plist 2)))

(defun llm-provider-utils-openai-arguments (args)
  "Convert ARGS to the OpenAI function calling spec.
ARGS is a list of llm argument plists.
Each plist has the structure:
  (:name STRING
   :type SYMBOL
   :description STRING
   :optional BOOLEAN
   :properties PLIST
   :enum VECTOR
   :items (PLIST :type SYMBOL :enum VECTOR :properties PLIST))

:type is a symbol, one of `string', `number', `boolean', `object', or
`array'."
  (let ((properties '())
        (required-names '()))
    (dolist (arg args)
      (let* ((arg-name (plist-get arg :name))
             (type (symbol-name (plist-get arg :type)))
             (description (plist-get arg :description))
             (required (not (plist-get arg :optional)))
             (enum (plist-get arg :enum))
             (items (plist-get arg :items))
             (obj-properties (llm-provider-utils-convert-to-serializable
                              (plist-get arg :properties)))
             (schema (list :type type)))

        ;; Add :description if present
        (when description
          (setq schema (plist-put schema :description description)))

        ;; Add :enum if present
        (when enum
          ;; Vectors generally serialize nicely to JSON arrays, but a list is fine too.
          (setq schema (plist-put schema :enum enum)))

        (when items
          (setq schema (plist-put schema
                                  :items
                                  (llm-provider-utils-convert-to-serializable items))))

        (when obj-properties
          (setq schema (plist-put schema :properties obj-properties)))

        ;; Track required argument names if :required is t
        (when required
          (push (if (symbolp arg-name)
                    (symbol-name arg-name)
                  arg-name) required-names))

        ;; Finally, put this schema into the :properties
        (setq properties
              (plist-put properties (llm-provider-utils--encolon arg-name)
                         schema))))
    ;; Build the final spec
    (let ((spec `(:type "object" :properties ,properties)))
      (when required-names
        (setq spec (plist-put spec :required (apply #'vector
                                                    (nreverse required-names)))))
      spec)))

(cl-defgeneric llm-provider-utils-openai-tool-spec (tool)
  "Convert TOOL to an Open AI function spec.")

;; The Open AI tool spec follows the JSON schema spec.  See
;; https://json-schema.org/understanding-json-schema.
(cl-defmethod llm-provider-utils-openai-tool-spec ((tool llm-tool))
  "Convert TOOL to an Open AI function spec.
Open AI's function spec is a standard way to do this, and will be
applicable to many endpoints.

This returns a JSON object (a list that can be converted to JSON)."
  `(:type "function"
          :function
          (:name ,(llm-tool-name tool)
                 :description ,(llm-tool-description tool)
                 :parameters ,(llm-provider-utils-openai-arguments
                               (llm-tool-args tool)))))

(defun llm-provider-utils-openai-collect-streaming-tool-uses (data)
  "Read Open AI compatible streaming output DATA to collect tool-uses."
  (let* ((num-index (+ 1 (assoc-default 'index (aref data 0))))
         (cvec (make-vector num-index nil)))
    (dotimes (i num-index)
      (setf (aref cvec i) (make-llm-provider-utils-tool-use)))
    (cl-loop for call in (append data nil) do
             (let* ((index (assoc-default 'index call))
                    (id (assoc-default 'id call))
                    (function (assoc-default 'function call))
                    (name (assoc-default 'name function))
                    (arguments (assoc-default 'arguments function)))
               (when id
                 (setf (llm-provider-utils-tool-use-id (aref cvec index)) id))
               (when name
                 (setf (llm-provider-utils-tool-use-name (aref cvec index)) name))
               (setf (llm-provider-utils-tool-use-args (aref cvec index))
                     (concat (llm-provider-utils-tool-use-args (aref cvec index))
                             arguments))))
    (cl-loop for call in (append cvec nil)
             do (setf (llm-provider-utils-tool-use-args call)
                      (json-parse-string (llm-provider-utils-tool-use-args call)
                                         :object-type 'alist))
             finally return (when (> (length cvec) 0)
                              (append cvec nil)))))

(defun llm-provider-utils-append-to-prompt (prompt output &optional tool-results role)
  "Append OUTPUT to PROMPT as an assistant interaction.

OUTPUT can be a string or a structure in the case of function calls.

TOOL-RESULTS is a list of results from the LLM output, if any.

ROLE will be `assistant' by default, but can be passed in for other roles."
  (setf (llm-chat-prompt-interactions prompt)
        (append (llm-chat-prompt-interactions prompt)
                (list (make-llm-chat-prompt-interaction
                       :role (or role
                                 (if tool-results 'tool-results 'assistant))
                       ;; If it is a structure, it will get converted to JSON,
                       ;; otherwise make sure it is a string.  For tool uses, we
                       ;; want it to be nil.
                       :content (if (or (not output)
                                        (and (not (stringp output))
                                             (not tool-results)))
                                    output
                                  (format "%s" output))
                       :tool-results tool-results)))))

(defun llm-provider-utils-process-result (provider prompt partial-result multi-output success-callback)
  "Process the RESPONSE from the provider for PROMPT.
This execute function calls if there are any, does any result
appending to the prompt, and returns an appropriate response for
the client.

PROVIDER is the struct that configures the use of the LLM.

TOOL-USES is a list of tool uses in the result.

PARTIAL-RESULT is the multipart result, without any tool results.

MULTI-OUTPUT is true if multiple outputs are expected to be passed to
SUCCESS-CALLBACK.

SUCCESS-CALLBACK is the callback that will be run when all functions
complete."
  (when (plist-get partial-result :text)
    (llm-provider-append-to-prompt provider prompt (plist-get partial-result :text)))
  (if-let ((tool-uses (plist-get partial-result :tool-uses)))
      ;; If we have tool uses, execute them, and on the callback, we will
      ;; populate the results.  We don't execute the callback here because it
      ;; will be done inside `llm-provider-utils-execute-tool-uses'.
      (llm-provider-utils-execute-tool-uses
       provider prompt tool-uses multi-output
       partial-result success-callback)
    (funcall success-callback
             (if multi-output partial-result
               (plist-get partial-result :text)))))

(defun llm-provider-utils-populate-tool-uses (provider prompt results-alist)
  "Append the results in RESULTS-ALIST to the prompt.

PROMPT is the prompt to populate into.

RESULTS-ALIST is a list of cons of function
calls (`llm-provider-utils-tool-use' structs) and their
results.

PROVIDER is the struct that configures the user of the LLM."
  (llm-provider-append-to-prompt
   provider prompt nil
   (mapcar (lambda (c) (make-llm-chat-prompt-tool-result
                        :call-id (llm-provider-utils-tool-use-id (car c))
                        :tool-name (llm-provider-utils-tool-use-name (car c))
                        :result (cdr c)))
           results-alist)))

(defun llm-provider-utils-final-multi-output-result (tool-results)
  "Return the final result from TOOL-RESULTS.

This transforms the plist so that:
1. We don't return an empty :text value.
2. We transform the :tool-uses to an alist of tool name to use."
  (cl-loop for (key value) on tool-results
           by 'cddr
           if (and (not (and (eq key :text) (equal value "")))
                   (member key '(:text :tool-uses :tool-results)))
           nconc (list key
                       (if (eq key :tool-uses)
                           (mapcar (lambda (tool-use)
                                     `(:name ,(llm-provider-utils-tool-use-name tool-use)
                                             :args ,(llm-provider-utils-tool-use-args tool-use)))
                                   value)
                         value))))

(defun llm-provider-utils--normalize-args (args)
  "Normalize ARGS to a form that can be passed to the user.

This will convert all :json-false and :false values to nil."
  (cond
   ((vectorp args) (vconcat (mapcar #'llm-provider-utils--normalize-args args)))
   ((listp args) (mapcar #'llm-provider-utils--normalize-args args))
   ((plistp args) (let (new-plist)
                    (map-do
                     (lambda (key value)
                       (setq new-plist
                             (plist-put new-plist
                                        key
                                        (llm-provider-utils--normalize-args value))))
                     args)))
   ((member args '(:json-false :false)) nil)
   (t args)))

(defun llm-provider-utils-execute-tool-uses (provider prompt tool-uses multi-output partial-result success-callback)
  "Execute TOOL-USES, a list of `llm-provider-utils-tool-use'.

A response suitable for returning to the client will be returned.

PROVIDER is the provider that supplied the response.

PROMPT was the prompt given to the provider, which will get
updated with the response from the LLM, and if there is a
function call, the result.

MULTI-OUTPUT is true if multiple outputs are expected to be passed to
SUCCESS-CALLBACK.

PARTIAL-RESULT is the result to return to the user, without the tool
call results.

SUCCESS-CALLBACK is the callback that will be run when all functions
have returned results."
  (llm-provider-populate-tool-uses provider prompt tool-uses)
  (let (results tool-use-and-results)
    (cl-loop
     for tool-use in tool-uses do
     (let* ((name (llm-provider-utils-tool-use-name tool-use))
            (arguments (llm-provider-utils-tool-use-args tool-use))
            (tool (seq-find
                   (lambda (f) (equal name (llm-tool-name f)))
                   (llm-chat-prompt-tools prompt)))
            (call-args (cl-loop for arg in (llm-tool-args tool)
                                collect (cdr (seq-find (lambda (a)
                                                         (eq (intern (plist-get arg :name))
                                                             (car a)))
                                                       arguments))))
            (end-func (lambda (result)
                        (llm--log
                         'api-funcall
                         :provider provider
                         :msg (format "%s --> %s"
                                      (format "%S" (cons name call-args))
                                      (format "%s" result)))
                        (push (cons name result) tool-use-and-results)
                        (push (cons tool-use result) results)
                        (when (= (length results) (length tool-uses))
                          (llm-provider-utils-populate-tool-uses
                           provider prompt results)
                          (funcall success-callback
                                   (if multi-output
                                       (llm-provider-utils-final-multi-output-result
                                        (append partial-result
                                                `(:tool-results ,tool-use-and-results)))
                                     tool-use-and-results))))))
       (if (llm-tool-async tool)
           (apply (llm-tool-function tool)
                  (append (list end-func) call-args))
         (funcall end-func (apply (llm-tool-function tool)
                                  (llm-provider-utils--normalize-args call-args))))))))


;; This is a useful method for getting out of the request buffer when it's time
;; to make callbacks.
(defun llm-provider-utils-callback-in-buffer (buf f &rest args)
  "Run F with ARGS in the context of BUF.
But if BUF has been killed, use a temporary buffer instead.
If F is nil, nothing is done."
  (when f
    (if (buffer-live-p buf)
        (with-current-buffer buf (apply f args))
      (with-temp-buffer (apply f args)))))

(defun llm-provider-utils-json-val (val)
  "Return VAL if it is not nil, otherwise return nil."
  (when (and val (not (eq val :null)))
    val))

(provide 'llm-provider-utils)
;;; llm-provider-utils.el ends here
