;;; llm-provider-utils.el --- Functions to make building providers easier -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

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
;; This file provides functions to help build providers. It should only be used
;; by modules implementing an LLM provider.

;;; Code:

(require 'llm)
(require 'llm-request)
(require 'seq)

(cl-defstruct llm-standard-provider
  "A struct indicating that this is a standard provider.
This is for dispatch purposes, so this contains no actual data.

This represents any provider, regardless of what it implements.

This should not be used outside of this file.")

(cl-defstruct (llm-standard-chat-provider (:include llm-standard-provider))
  "A struct for indicating a provider that implements chat.")

(cl-defstruct (llm-standard-full-provider (:include llm-standard-chat-provider))
  "A struct for providers that implements chat and embeddings.")

;; Methods necessary for both embedding and chat requests.

(cl-defgeneric llm-provider-request-prelude (provider)
  "Execute any prelude code necessary before running a request.")

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
  "By default, use the same URL as normal chat."
  (llm-provider-chat-url provider))

(cl-defgeneric llm-provider-chat-timeout (provider)
  "Return the seconds of timeout for PROVIDER.
Return nil for the standard timeout.")

(cl-defmethod llm-provider-chat-timeout ((_ llm-standard-provider))
  "By default, the standard provider has the standard timeout."
  nil)

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
FUNC-RESULTS is a list of function results, if any.")

(cl-defmethod llm-provider-append-to-prompt ((_ llm-standard-chat-provider) prompt result
                                             &optional func-results)
  "By default, the standard provider appends to the prompt."
  (llm-provider-utils-append-to-prompt prompt result func-results))

(cl-defgeneric llm-provider-extract-partial-response (provider response)
  "Extract the result string from partial RESPONSE for the PROVIDER.
This should return the entire string so far.")

;; Methods for chat function calling

(cl-defgeneric llm-provider-extract-function-calls (provider response)
  "Return the function calls from RESPONSE for the PROVIDER.
If there are no function calls, return nil.  If there are
function calls, return a list of
`llm-provider-utils-function-call'.")

(cl-defmethod llm-provider-extract-function-calls ((_ llm-standard-chat-provider) _)
  "By default, the standard provider has no function call extractor."
  nil)

(cl-defgeneric llm-provider-populate-function-calls (provider prompt calls)
  "For PROVIDER, in PROMPT, record that function CALLS were received.
This is the recording before the calls were executed.
CALLS are a list of `llm-provider-utils-function-call'.")

(cl-defgeneric llm-provider-extract-streamed-function-calls (provider response)
  "Extract the result string from partial RESPONSE for the PROVIDER.")

(cl-defmethod llm-provider-extract-streamed-function-calls ((_ llm-standard-chat-provider) _)
  "By default, there are no function calls."
  nil)

;; Standard provider implementations of llm functionality

(cl-defmethod llm-embedding ((provider llm-standard-full-provider) string)
  (llm-provider-request-prelude provider)
  (let ((response (llm-request-sync (llm-provider-embedding-url provider)
                                    :timeout (llm-provider-chat-timeout provider)
                                    :headers (llm-provider-headers provider)
                                    :data (llm-provider-embedding-request provider string))))
    (if-let ((err-msg (llm-provider-embedding-extract-error provider response)))
        (error err-msg)
      (llm-provider-embedding-extract-result provider response))))

(cl-defmethod llm-embedding-async ((provider llm-standard-full-provider) string vector-callback error-callback)
  (llm-provider-request-prelude provider)
  (let ((buf (current-buffer)))
    (llm-request-async
     (llm-provider-embedding-url provider)
     :headers (llm-provider-headers provider)
     :data (llm-provider-embedding-request provider string)
     :on-success (lambda (data)
                   (if-let ((err-msg (llm-provider-embedding-extract-error provider data)))
                       (llm-request-callback-in-buffer
                        buf error-callback 'error
                        err-msg)
                     (llm-provider-utils-callback-in-buffer
                      buf vector-callback
                      (llm-provider-embedding-extract-result provider data)))
                   (kill-current-buffer))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-embedding-extract-error
                         provider data)
                        "Unknown error")))
                 (kill-current-buffer)))))

(cl-defmethod llm-chat ((provider llm-standard-chat-provider) prompt)
  (llm-provider-request-prelude provider)
  (let ((response (llm-request-sync (llm-provider-chat-url provider)
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
    (llm-request-async
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
                       (llm-provider-extract-function-calls provider data))))
                   (kill-current-buffer))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-chat-extract-error
                         provider data)
                        "Unknown error")))
                 (kill-current-buffer)))))

(cl-defmethod llm-chat-streaming ((provider llm-standard-chat-provider) prompt partial-callback
                                  response-callback error-callback)
  (llm-provider-request-prelude provider)
  (let ((buf (current-buffer)))
    (llm-request-async
     (llm-provider-chat-streaming-url provider)
     :headers (llm-provider-headers provider)
     :data (llm-provider-chat-request provider prompt t)
     :on-partial
     (lambda (data)
       ;; We won't have a result if this is a streaming function call,
       ;; so we don't call on-partial in that case.
       (when-let ((result (llm-provider-extract-partial-response provider data)))
         ;; Let's not be so strict, a partial response empty string
         ;; should be equivalent to nil.
         (when (> (length result) 0)
           (llm-provider-utils-callback-in-buffer buf partial-callback result))))
     :on-success-raw
     (lambda (data)
       (llm-provider-utils-callback-in-buffer
        buf response-callback
        (llm-provider-utils-process-result
         provider prompt
         (llm-provider-extract-partial-response provider data)
         (llm-provider-extract-streamed-function-calls provider data)))
       (kill-current-buffer))
     :on-error (lambda (_ data)
                 (llm-provider-utils-callback-in-buffer
                  buf error-callback 'error
                  (if (stringp data)
                      data
                    (or (llm-provider-chat-extract-error
                         provider data)
                        "Unknown error")))
                 (kill-current-buffer)))))

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
If there is an assistance response, do nothing."
  (unless (seq-some
           (lambda (interaction)
             (eq (llm-chat-prompt-interaction-role interaction) 'assistant))
           (llm-chat-prompt-interactions prompt))
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
                (llm-chat-prompt-interactions prompt)))))))

(defun llm-provider-utils-combine-to-user-prompt (prompt &optional example-prelude)
  "Add context and examples to a user prompt in PROMPT.
This should be used for providers that do not have a notion of a system prompt."
  (when (= (length (llm-chat-prompt-interactions prompt)) 1)
    (when-let ((system-content (llm-provider-utils-get-system-prompt prompt example-prelude)))
      (setf (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt)))
            (concat system-content
                    "\n"
                    (llm-chat-prompt-interaction-content (car (llm-chat-prompt-interactions prompt))))))))

(defun llm-provider-utils-collapse-history (prompt &optional history-prelude)
  "Collapse history to a single prompt.
This is useful for providers that cannot handle conversations.
Essentially it's a way to fake conversation. Caution: tokens will
eventually run out, though, so this isn't a sustainable way to do
things.  Providers should probably issue a warning when using this."
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
    ((string-match-p "llama\s*2" model) 4096)
    ((string-match-p "llama" model) 2048)
    ((string-match-p "starcoder" model) 8192))))

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

ROLE will be `assistant' by default, but can be passed in for other roles."
  (setf (llm-chat-prompt-interactions prompt)
        (append (llm-chat-prompt-interactions prompt)
                (list (make-llm-chat-prompt-interaction
                       :role (if func-results
                                 'function
                               (or role 'assistant))
                       :content output
                       :function-call-result func-results)))))

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

FUNCALLS is a list of function calls, if any.

TEXT is the text output from the provider, if any.  There should
be either FUNCALLS or TEXT."
  (if-let ((funcalls funcalls))
      ;; If we have function calls, execute them and return the results, and
      ;; it talso takes care of updating the prompt.
      (llm-provider-utils-execute-function-calls provider prompt funcalls)
    (llm-provider-append-to-prompt provider prompt text)
    text))

(defun llm-provider-utils-populate-function-results (provider prompt func result)
  "Append the RESULT of FUNC to PROMPT.
FUNC is a `llm-provider-utils-function-call' struct."
  (llm-provider-append-to-prompt
   provider prompt result
   (make-llm-chat-prompt-function-call-result
    :call-id (llm-provider-utils-function-call-id func)
    :function-name (llm-provider-utils-function-call-name func)
    :result result)))

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
  (cl-loop for func in funcalls collect
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
                     (llm-provider-utils-populate-function-results
                      provider prompt func result)
                     (llm--log
                      'api-funcall
                      :provider provider
                      :msg (format "%s --> %s"
                                   (format "%S"
                                           (cons (llm-function-call-name function)
                                                 args))
                                   (format "%s" result)))
                     result)))))


;; This is a useful method for getting out of the request buffer when it's time
;; to make callbacks.
(defun llm-provider-utils-callback-in-buffer (buf f &rest args)
  "Run F with ARSG in the context of BUF.
But if BUF has been killed, use a temporary buffer instead.
If F is nil, nothing is done."
  (when f
    (if (buffer-live-p buf)
        (with-current-buffer buf (apply f args))
      (with-temp-buffer (apply f args)))))

(provide 'llm-provider-utils)
;;; llm-provider-utils.el ends here
