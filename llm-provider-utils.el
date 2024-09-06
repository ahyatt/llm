;;; llm-provider-utils.el --- Functions to make building providers easier -*- lexical-binding: t; package-lint-main-file: "llm.el"; ; byte-compile-docstring-max-column: 200-*-

;; Copyright (c) 2023, 2024  Free Software Foundation, Inc.

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
(require 'seq)

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
(cl-defgeneric llm-provider-embedding-url (provider)
  "Return the URL for embeddings for the PROVIDER.")

(cl-defgeneric llm-provider-embedding-request (provider string)
  "Return the request for the PROVIDER for STRING.")

(cl-defgeneric llm-provider-embedding-extract-error (provider response)
  "Return an error message from RESPONSE for the PROVIDER.

RESPONSE is a parsed JSON object.

Return nil if there is no error.")

(cl-defmethod llm-provider-embedding-extract-error ((_ llm-standard-full-provider) _)
  "By default, the standard provider has no error extractor."
  nil)

(cl-defgeneric llm-provider-embedding-extract-result (provider response)
  "Return the result from RESPONSE for the PROVIDER.")

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
        ;; We need to merge the parameteres individually.
        (seq-union (llm-chat-prompt-non-standard-params prompt)
                   (llm-standard-chat-provider-default-chat-non-standard-params provider)
                   (lambda (a b)
                     (equal (car a) (car b))))))

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

(cl-defgeneric llm-provider-append-to-prompt (provider prompt result func-results)
  "Append RESULT to PROMPT for the PROVIDER.

PROMPT is the prompt that was already sent to the provider.

FUNC-RESULTS is a list of function results, if any.")

(cl-defmethod llm-provider-append-to-prompt ((_ llm-standard-chat-provider) prompt result
                                             &optional func-results)
  ;; By default, we just append to the prompt.
  (llm-provider-utils-append-to-prompt prompt result func-results))

(cl-defgeneric llm-provider-streaming-media-handler (provider msg-receiver fc-receiver err-receiver)
  "Define how to handle streaming media for the PROVIDER.

This should return a cons of the media type and an instance that
handle objects of that type.

The handlers defined can call MSG-RECEIVER when they receive part
of a text message for the client (a chat response).  If they
receive a function call, they should call FC-RECEIVER with the
function call.  If they receive an error, they should call
ERR-RECEIVER with the error message.")

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-standard-chat-provider) _ _ _)
  "By default, the standard provider has no streaming media handler."
  nil)

;; Methods for chat function calling

(cl-defgeneric llm-provider-extract-function-calls (provider response)
  "Return the function call results from RESPONSE for the PROVIDER.

If there are no function call results, return nil.  If there are
function call results, return a list of
`llm-provider-utils-function-call'.")

(cl-defmethod llm-provider-extract-function-calls ((_ llm-standard-chat-provider) _)
  "By default, the standard provider has no function call extractor."
  nil)

(cl-defgeneric llm-provider-populate-function-calls (provider prompt calls)
  "For PROVIDER, in PROMPT, record function call execution.
This is the recording before the calls were executed, in the prompt.
CALLS are a list of `llm-provider-utils-function-call'.")

(cl-defgeneric llm-provider-collect-streaming-function-data (provider data)
  "Transform a list of streaming function call DATA responses.

PROVIDER is the struct specifying the LLM provider and its configuration.

The DATA responses are a list of whatever is sent to the function
call handler in `llm-provider-streaming-media-handler'.  This should
return a list of `llm-chat-function-call' structs.")

(cl-defmethod llm-provider-collect-streaming-function-data ((_ llm-standard-chat-provider) _)
  ;; by default, there is no function calling
  nil)

;; Standard provider implementations of llm functionality

(cl-defmethod llm-embedding ((provider llm-standard-full-provider) string)
  (llm-provider-request-prelude provider)
  (let ((response (llm-request-plz-sync
                   (llm-provider-embedding-url provider)
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
     (llm-provider-embedding-url provider)
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

(cl-defmethod llm-chat ((provider llm-standard-chat-provider) prompt)
  (llm-provider-request-prelude provider)
  (let ((response (llm-request-plz-sync (llm-provider-chat-url provider)
                                        :headers (llm-provider-headers provider)
                                        :data (llm-provider-chat-request provider prompt nil))))
    (if-let ((err-msg (llm-provider-chat-extract-error provider response)))
        (error err-msg)
      (llm-provider-utils-process-result provider prompt
                                         (llm-provider-chat-extract-result
                                          provider response)
                                         (llm-provider-extract-function-calls
                                          provider response)))))

(cl-defmethod llm-chat-async ((provider llm-standard-chat-provider) prompt success-callback
                              error-callback)
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
                     (llm-provider-utils-callback-in-buffer
                      buf success-callback
                      (llm-provider-utils-process-result
                       provider prompt
                       (llm-provider-chat-extract-result provider data)
                       (llm-provider-extract-function-calls provider data)))))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-chat-extract-error
                         provider data)
                        "Unknown error")))))))

(cl-defmethod llm-chat-streaming ((provider llm-standard-chat-provider) prompt partial-callback
                                  response-callback error-callback)
  (llm-provider-request-prelude provider)
  (let ((buf (current-buffer))
        (current-text "")
        (fc nil))
    (llm-request-plz-async
     (llm-provider-chat-streaming-url provider)
     :headers (llm-provider-headers provider)
     :data (llm-provider-chat-request provider prompt t)
     :media-type (llm-provider-streaming-media-handler
                  provider
                  (lambda (s)
                    (when (> (length s) 0)
                      (setq current-text
                            (concat current-text s))
                      (when partial-callback
                        (llm-provider-utils-callback-in-buffer
                         buf partial-callback current-text))))
                  (lambda (fc-new) (push fc-new fc))
                  (lambda (err)
                    (llm-provider-utils-callback-in-buffer
                     buf error-callback 'error
                     err)))
     :on-success
     (lambda (_)
       ;; We don't need the data at the end of streaming, so we can ignore it.
       (llm-provider-utils-callback-in-buffer
        buf response-callback
        (llm-provider-utils-process-result
         provider prompt
         current-text
         (when fc
           (llm-provider-collect-streaming-function-data
            provider (nreverse fc))))))
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
  (when-let ((system-content (llm-provider-utils-get-system-prompt prompt example-prelude)))
    (setf (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt)))
          (concat system-content
                  "\n"
                  (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt))))
          (llm-chat-prompt-context prompt) nil
          (llm-chat-prompt-examples prompt) nil)))

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

(defun llm-provider-utils-model-token-limit (model)
  "Return the token limit for MODEL."
  (let ((model (downcase model)))
    (cond
     ((string-match-p "mistral-7b" model) 8192)
     ((string-match-p "mistral" model) 8192)
     ((string-match-p "mixtral-45b" model) 131072)
     ((string-match-p "mixtral" model) 131072)
     ((string-match-p "falcon" model) 2048)
     ((string-match-p "orca 2" model) 4096)
     ((string-match-p "orca" model) 2048)
     ((string-match-p "llama\s*3" model) 131072)
     ((string-match-p "llama\s*2" model) 4096)
     ((string-match-p "llama" model) 2048)
     ((string-match-p "starcoder" model) 8192)
     ((string-match-p "gemma" model) 8192)
     ;; default to the smallest context window, 2048
     (t 2048))))

(defun llm-provider-utils-openai-arguments (args)
  "Convert ARGS to the Open AI function calling spec.
ARGS is a list of `llm-function-arg' structs."
  (let ((required (mapcar
                   #'llm-function-arg-name
                   (seq-filter #'llm-function-arg-required args))))
    (append
     `((type . object)
       (properties
        .
        ,(mapcar (lambda (arg)
                   `(,(llm-function-arg-name arg) .
                     ,(if (and (listp (llm-function-arg-type arg))
                               (llm-function-arg-p (car (llm-function-arg-type arg))))
                          (llm-provider-utils-openai-arguments (llm-function-arg-type arg))
                        (append
                         `((type .
                                 ,(pcase (llm-function-arg-type arg)
                                    ('string 'string)
                                    ('integer 'integer)
                                    ('float 'number)
                                    ('boolean 'boolean)
                                    ((cl-type cons)
                                     (pcase (car (llm-function-arg-type arg))
                                       ('or (cdr (llm-function-arg-type arg)))
                                       ('list 'array)
                                       ('enum 'string)))
                                    (_ (error "Unknown argument type: %s" (llm-function-arg-type arg))))))
                         (when (llm-function-arg-description arg)
                           `((description
                              .
                              ,(llm-function-arg-description arg))))
                         (when (and (eq 'cons
                                        (type-of (llm-function-arg-type arg))))
                           (pcase (car (llm-function-arg-type arg))
                             ('enum `((enum
                                       .
                                       ,(cdr (llm-function-arg-type arg)))))
                             ('list
                              `((items .
                                       ,(if (llm-function-arg-p
                                             (cadr (llm-function-arg-type arg)))
                                            (llm-provider-utils-openai-arguments
                                             (cdr (llm-function-arg-type arg)))
                                          `((type . ,(cadr (llm-function-arg-type arg))))))))))))))
                 args)))
     (when required
       `((required . ,required))))))

;; The Open AI function calling spec follows the JSON schema spec.
;; See https://json-schema.org/understanding-json-schema.
(defun llm-provider-utils-openai-function-spec (call)
  "Convert `llm-function-call' CALL to an Open AI function spec.
Open AI's function spec is a standard way to do this, and will be
applicable to many endpoints.

This returns a JSON object (a list that can be converted to JSON)."
  `((type . function)
    (function
     .
     ,(append
       `((name . ,(llm-function-call-name call))
         (description . ,(llm-function-call-description call)))
       (when (llm-function-call-args call)
         `((parameters
            .
            ,(llm-provider-utils-openai-arguments (llm-function-call-args call)))))))))

(defun llm-provider-utils-append-to-prompt (prompt output &optional func-results role)
  "Append OUTPUT to PROMPT as an assistant interaction.

OUTPUT can be a string or a structure in the case of function calls.

FUNC-RESULTS is a list of function results resulting from the LLM
output, if any.

ROLE will be `assistant' by default, but can be passed in for other roles."
  (setf (llm-chat-prompt-interactions prompt)
        (append (llm-chat-prompt-interactions prompt)
                (list (make-llm-chat-prompt-interaction
                       :role (or role
                                 (if func-results 'function 'assistant))
                       ;; If it is a structure, it will get converted to JSON,
                       ;; otherwise make sure it is a string.
                       :content (if (listp output)
                                    output
                                  (format "%s" output))
                       :function-call-results func-results)))))

(cl-defstruct llm-provider-utils-function-call
  "A struct to hold information about a function call.
ID is a call ID, which is optional.
NAME is the function name.
ARG is an alist of arguments to values."
  id name args)

(defun llm-provider-utils-process-result (provider prompt text funcalls)
  "Process the RESPONSE from the provider for PROMPT.
This execute function calls if there are any, does any result
appending to the prompt, and returns an appropriate response for
the client.

PROVIDER is the struct that configures the use of the LLM.

FUNCALLS is a list of function calls, if any.

TEXT is the text output from the provider, if any.  There should
be either FUNCALLS or TEXT."
  (if-let ((funcalls funcalls))
      ;; If we have function calls, execute them and return the results, and
      ;; it talso takes care of updating the prompt.
      (let ((results-alist
             (llm-provider-utils-execute-function-calls provider prompt funcalls)))
        (llm-provider-utils-populate-function-results
         provider prompt results-alist)
        (mapcar #'cdr results-alist))
    ;; We probably shouldn't be called if text is nil, but if we do,
    ;; we shouldn't add something invalid to the prompt.
    (when text
      (llm-provider-append-to-prompt provider prompt text))
    text))

(defun llm-provider-utils-populate-function-results (provider prompt results-alist)
  "Append the results in RESULTS-ALIST to the prompt.

PROMPT is the prompt to populate into.

RESULTS-ALIST is a list of cons of function
calls (`llm-provider-utils-function-call' structs) and their
results.

PROVIDER is the struct that configures the user of the LLM."
  (llm-provider-append-to-prompt
   provider prompt nil
   (mapcar (lambda (c) (make-llm-chat-prompt-function-call-result
                        :call-id (llm-provider-utils-function-call-id (car c))
                        :function-name (llm-provider-utils-function-call-name (car c))
                        :result (cddr c)))
           results-alist)))

(defun llm-provider-utils-execute-function-calls (provider prompt funcalls)
  "Execute FUNCALLS, a list of `llm-provider-utils-function-calls'.

A response suitable for returning to the client will be returned.

PROVIDER is the provider that supplied the response.

PROMPT was the prompt given to the provider, which will get
updated with the response from the LLM, and if there is a
function call, the result.

This returns the response suitable for output to the client; a
cons of functions called and their output."
  (llm-provider-populate-function-calls provider prompt funcalls)
  (cl-loop
   for func in funcalls collect
   (cons
    func
    (let* ((name (llm-provider-utils-function-call-name func))
           (arguments (llm-provider-utils-function-call-args func))
           (function (seq-find
                      (lambda (f) (equal name (llm-function-call-name f)))
                      (llm-chat-prompt-functions prompt))))
      (cons name
            (let* ((args (cl-loop for arg in (llm-function-call-args function)
                                  collect (cdr (seq-find (lambda (a)
                                                           (eq (intern
                                                                (llm-function-arg-name arg))
                                                               (car a)))
                                                         arguments))))
                   (result (apply (llm-function-call-function function) args)))
              (llm--log
               'api-funcall
               :provider provider
               :msg (format "%s --> %s"
                            (format "%S"
                                    (cons (llm-function-call-name function)
                                          args))
                            (format "%s" result)))
              result))))))


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

(provide 'llm-provider-utils)
;;; llm-provider-utils.el ends here
