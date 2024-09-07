;;; llm.el --- Interface to pluggable llm backends -*- lexical-binding: t; byte-compile-docstring-max-column: 200 -*-

;; Copyright (c) 2023, 2024  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm
;; Package-Requires: ((emacs "28.1") (plz "0.8"))
;; Package-Version: 0.17.4
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
;; This file defines a generic interface for LLMs (large language models), and
;; functionality they can provide.  Not all LLMs will support all of these, but
;; programs that want to integrate with LLMs can code against the interface, and
;; users can then choose the LLMs they want to use.  It's advisable to have the
;; possibility of using multiple LLMs when that make sense for different
;; functionality.
;;
;; Users should require this module and then the module of the LLM they want to
;; use.
;;
;; Not all LLMs might be able to do everything, so clients need to catch any
;; signals thrown with symbol `not-implemented', and surface an error to the
;; user that the LLM they have chosen cannot be used for that functionality.

;;; Code:

(require 'cl-lib)

(defgroup llm nil
  "Interface to pluggable llm backends."
  :group 'external)

(defcustom llm-warn-on-nonfree t
  "Whether to issue a warning when using a non-free LLM."
  :type 'boolean)

(defcustom llm-log nil
  "Whether to log messages to the llm module.
Logs will be in the buffer *llm log*.  This should only be used
for debugging, because the log buffer will grow without bound."
  :type 'boolean)

(defun llm--warn-on-nonfree (name tos)
  "Issue a warning if `llm-warn-on-nonfree' is non-nil.
NAME is the human readable name of the LLM (e.g \"Open AI\").

TOS is the URL of the terms of service for the LLM.

All non-free LLMs should call this function on each llm function
invocation."
  (when llm-warn-on-nonfree
    (lwarn 'llm :warning "%s API is not free software, and your freedom to use it is restricted.
See %s for the details on the restrictions on use." name tos)))

(cl-defstruct llm-chat-prompt
  "This stores all the information needed for a structured chat prompt.

Use of this directly is deprecated, instead use `llm-make-chat-prompt'."
  context examples interactions functions temperature max-tokens non-standard-params)

(cl-defstruct llm-chat-prompt-interaction
  "This defines a single interaction given as part of a chat prompt.
ROLE can a symbol, of either `user', `assistant', or `function'.

FUNCTION-CALL-RESULTS is a list of structs of type
`llm-chat-prompt-function-call-results', which is only populated
if `role' is `function'.  It stores the results of the function
calls."
  role content  function-call-results)

(cl-defstruct llm-chat-prompt-function-call-result
  "This defines the result from a function call.

CALL-ID is an ID for this function call, if available.

FUNCTION-NAME is the name of the function.  This is required.

RESULT is the result of the function call.  This is required."
  call-id function-name result)

(cl-defstruct llm-function-call
  "This is a struct to represent a function call the LLM can make.

All fields are required.

FUNCTION is a function to call.

NAME is a human readable name of the function.

DESCRIPTION is a human readable description of the function.

ARGS is a list of `llm-function-arg' structs."
  function
  name
  description
  args)

(cl-defstruct llm-function-arg
  "An argument to an `llm-function-call'.

NAME is the name of the argument.

DESCRIPTION is a human readable description of the argument.  It
can be nil for enums.

TYPE is the type of the argument.  It can be one of `string',
`integer', `float', `boolean' or the special lists, `(or <type1>
<type2> ... <typen>)', `(enum <string1> <string2> ...
<stringn>)', or `(list <type>)'.

REQUIRED is whether this is required or not."
  name
  description
  type
  required)

(cl-defun llm--log (type &key provider prompt msg)
  "Log a MSG of TYPE, given PROVIDER, PROMPT, and MSG.
These are all optional, each one should be the normal meaning of
this variable in this library.  TYPE can be one of `api-send',
`api-receive-parial', `api-receive-complete', `api-error', or
`prompt-append'."
  (when llm-log
    (with-current-buffer (get-buffer-create "*llm log*")
      (goto-char (point-max))
      (let ((marker (make-marker)))
        (set-marker marker (point))
        (insert (format "[%s] %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        (pcase type
                          ('api-send (format
                                      "[Emacs --> %s]:\n%s"
                                      (llm-name provider)
                                      (llm-chat-prompt-to-text prompt)))
                          ('api-receive-partial
                           (format "[%s --> Emacs] (partial): %s"
                                   (llm-name provider)
                                   msg))
                          ('api-receive
                           (format "[%s --> Emacs]: %s"
                                   (llm-name provider) msg))
                          ('api-error "[Error]: %s" msg)
                          ('api-funcall "[%s execution] %s" msg)
                          ('prompt-append (format "[Append to conversation]: %s"
                                                  msg)))))))))

(defun llm-make-simple-chat-prompt (text)
  "Create a `llm-chat-prompt' with TEXT sent to the LLM provider.
This is a helper function for when you just need to send text to
an LLM, and don't need the more advanced features that the
`llm-chat-prompt' struct makes available.

This is deprecated, and you should use `llm-make-chat-prompt'
instead."
  (llm-make-chat-prompt text))

(cl-defun llm-make-chat-prompt (text &key context examples functions
                                     temperature max-tokens
                                     non-standard-params)
  "Create a `llm-chat-prompt' with TEXT sent to the LLM provider.

This is the most correct and easy way to create an
`llm-chat-prompt', and should suffice for almost all uses.

Note that this should be called just once per interactive session
with an LLM, and the prompt re-used for all subsequent
interactions.  The reason for this is that some LLMs may store
data about previous interactions in opaque ways, so they can only
be populated once.  Therefore, if PREVIOUS-INTERACTIONS is
populated, a best effort is made to do something reasonable, but
it may not be quite the same on all providers as the prompt
mutating in terms of an actual conversation.

TEXT is the latest user input to the LLM, the thing to be
responded to.  This is required.  This can also be a string, in
which case it represents the chat history, starting with the
user's initial chat, followed by the response, and so on.  If it
is a list, it MUST be an odd number, since the presumption is
that it ends with the user's latest input to the LLM.

CONTEXT is a string given to the LLM as context for the entire
interaction, such as instructions to the LLM on how to reply,
persona, information on the user, or anything else that applies
to the chat as a whole.  This is optional.

EXAMPLES is a list of conses, where the car is an example
inputs, and cdr is the corresponding example outputs.  This is optional.

FUNCTIONS is a list of `llm-function-call' structs.  These may be
called IF the LLM supports them.  If the LLM does not support
them, a `not-implemented' signal will be thrown.  This is
optional.  When this is given, the LLM will either call the
function or return text as normal, depending on what the LLM
decides.

TEMPERATURE is a floating point number with a minimum of 0, and
maximum of 1, which controls how predictable the result is, with
0 being the most predicatable, and 1 being the most creative.
This is not required.

MAX-TOKENS is the maximum number of tokens to generate.  This is optional.

CONTEXT, EXAMPLES, FUNCTIONS, TEMPERATURE, and MAX-TOKENS are
usually turned into part of the interaction, and if so, they will
be put in the first interaction of the prompt (before anything in
PREVIOUS-INTERACTIONS).

NON-STANDARD-PARAMS is an alist of other options that the
provider may or may not know how to handle.  These are expected
to be provider specific.  Don't use this if you want the prompt
to be used amongst different providers, because it is likely to
cause a request error.  The cars of the alist are strings and the
cdrs can be strings or numbers.  This is optional."
  (unless text
    (error "TEXT is required"))
  (when (and (listp text) (zerop (mod (length text) 2)))
    (error "TEXT, as a list, must have an odd number of elements"))
  (make-llm-chat-prompt
   :context context
   :examples examples
   :interactions (seq-map-indexed (lambda (s i)
                                    (make-llm-chat-prompt-interaction
                                     :role (if (zerop (mod i 2)) 'user 'assistant)
                                     :content s))
                                  (if (listp text) text (list text)))
   :functions functions
   :temperature temperature
   :max-tokens max-tokens
   :non-standard-params non-standard-params))

(defun llm-chat-prompt-append-response (prompt response &optional role)
  "Append a new RESPONSE to PROMPT, to continue a conversation.
ROLE default to `user', which should almost always be what is needed."
  (setf (llm-chat-prompt-interactions prompt)
        (append (llm-chat-prompt-interactions prompt)
                (list (make-llm-chat-prompt-interaction :role (or role 'user)
                                                        :content response)))))

(cl-defgeneric llm-nonfree-message-info (provider)
  "If PROVIDER is non-free, return info for a warning.
This should be the string URL of the terms of service.

If the LLM is free and has no restrictions on use, this should
return nil.  Since this function already returns nil, there is no
need to override it."
  (ignore provider)
  nil)

(cl-defgeneric llm-chat (provider prompt)
  "Return a response to PROMPT from PROVIDER.
PROMPT is a `llm-chat-prompt'.

The response is a string response by the LLM when functions are
not called.  If functions are called, the response is a list of
conses of the function named called (as a symbol), and the
corresponding result from calling it.

The prompt's interactions list will be updated to encode the
conversation so far."
  (ignore provider prompt)
  (signal 'not-implemented nil))

(cl-defmethod llm-chat ((_ (eql nil)) _)
  "Catch trivial configuration mistake."
  (error "LLM provider was nil.  Please set the provider in the application you are using"))

(cl-defmethod llm-chat :before (provider _)
  "Issue a warning if the LLM is non-free."
  (when-let (info (llm-nonfree-message-info provider))
    (llm--warn-on-nonfree (llm-name provider) info)))

(cl-defmethod llm-chat :around (provider prompt)
  "Log the input to llm-chat."
  (llm--log 'api-send :provider provider :prompt prompt)
  ;; We set the debug flag to nil around the next-method so that we don't log
  ;; twice.
  (let* ((llm-log-orig llm-log)
         (llm-log nil)
         (result (cl-call-next-method))
         (llm-log llm-log-orig))
    (when (stringp result)
      (llm--log 'api-receive :provider provider :msg result))
    result))

(cl-defgeneric llm-chat-async (provider prompt response-callback error-callback)
  "Call RESPONSE-CALLBACK with a response to PROMPT from PROVIDER.

The response is a string response by the LLM when functions are
not called.  If functions are called, the response is a list of
conses of the function named called (as a symbol), and the
corresponding result from calling it.

PROMPT is a `llm-chat-prompt'.

RESPONSE-CALLBACK receives the final text.

ERROR-CALLBACK receives the error response.

The prompt's interactions list will be updated to encode the
conversation so far.

This returns an object representing the async request, which can
be passed to `llm-cancel-request'."
  ;; By default, you can turn a streaming call into an async call, so we can
  ;; fall back to streaming if async is not populated.
  ;; However, first, we don't want to log twice, so let's delete the last log so that llm-chat-streaming will
  (llm-chat-streaming provider prompt
                      ;; Do nothing on partial callback
                      nil
                      (lambda (text)
                        (funcall response-callback text))
                      (lambda (err msg) (funcall error-callback err msg))))

(cl-defmethod llm-chat-async :around (provider prompt response-callback error-callback)
  "Log the input to llm-chat-async."
  (llm--log 'api-send :provider provider :prompt prompt)
  (let* ((new-response-callback (lambda (response)
                                  (llm--log 'api-receive :provider provider :msg response)
                                  (let ((llm-log nil))
                                    (funcall response-callback response))))
         (new-error-callback (lambda (type err)
                               (llm--log 'api-error :provider provider
                                         :msg (format "Error type: %s, message: %s" type err))
                               (let ((llm-log nil))
                                 (funcall error-callback type err))))
         (llm-log nil)
         (result (cl-call-next-method provider prompt
                                      new-response-callback
                                      new-error-callback)))
    result))

(cl-defgeneric llm-chat-streaming (provider prompt partial-callback response-callback error-callback)
  "Stream a response to PROMPT from PROVIDER.
PROMPT is a `llm-chat-prompt'.

The response is a string response by the LLM when functions are
not called.  If functions are called, the response is a list of
conses of the function named called (as a symbol), and the
corresponding result from calling it.

PARTIAL-CALLBACK is called with the output of the string response
as it is built up.  The callback is called with the entire
response that has been received, as it is streamed back.  It is
not guaranteed to be called with the complete response before
RESPONSE-CALLBACK is called.  This can be nil, so that
implementations can just define this method which can be called
by `llm-chat-async', but with a nil value here to never get
partial callbacks.

RESPONSE-CALLBACK receives the each piece of the string response.
It is called once after the response has been completed, with the
final text.

ERROR-CALLBACK receives the error response.

The prompt's interactions list will be updated to encode the
conversation so far.

This returns an object representing the async request, which can
be passed to `llm-cancel-request'."
  (ignore provider prompt partial-callback response-callback error-callback)
  (signal 'not-implemented nil))

(cl-defmethod llm-chat-streaming ((_ (eql nil)) _ _ _ _)
  "Catch trivial configuration mistake."
  (error "LLM provider was nil.  Please set the provider in the application you are using"))

(cl-defmethod llm-chat-streaming :before (provider _ _ _ _)
  "Issue a warning if the LLM is non-free."
  (when-let (info (llm-nonfree-message-info provider))
    (llm--warn-on-nonfree (llm-name provider) info)))

(cl-defmethod llm-chat-streaming :around (provider prompt partial-callback response-callback error-callback)
  "Log the input to llm-chat-async."
  (llm--log 'api-send :provider provider :prompt prompt)
  ;; We need to wrap the callbacks before we set llm-log to nil.
  (let* ((new-partial-callback (lambda (response)
                                 (when partial-callback
                                   (let ((llm-log nil))
                                     (funcall partial-callback response)))))
         (new-response-callback (lambda (response)
                                  (llm--log 'api-receive :provider provider :msg response)
                                  (let ((llm-log nil))
                                    (funcall response-callback response))))
         (new-error-callback (lambda (type err)
                               (llm--log 'api-error :provider provider
                                         :msg (format "Error type: %s, message: %s" type err))
                               (let ((llm-log nil))
                                 (funcall error-callback type err))))
         (llm-log nil)
         (result (cl-call-next-method provider prompt new-partial-callback
                                      new-response-callback
                                      new-error-callback)))
    result))

(defun llm-chat-streaming-to-point (provider prompt buffer point finish-callback)
  "Stream the llm output of PROMPT to POINT in BUFFER.
PROVIDER is the backend provider of the LLM functionality.
FINISH-CALLBACK is called with no arguments when the output has finished.
This returns an object representing the async request, which can
be passed to `llm-cancel-request'."
  (with-current-buffer buffer
    (save-excursion
      (let ((start (make-marker))
            (end (make-marker)))
        (set-marker start point)
        (set-marker end point)
        (set-marker-insertion-type start nil)
        (set-marker-insertion-type end t)
        (cl-flet ((insert-text (text)
                    ;; Erase and insert the new text between the marker cons.
                    (with-current-buffer (marker-buffer start)
                      (save-excursion
                        (goto-char start)
                        (let* ((current-text
                                (buffer-substring-no-properties start end))
                               (common-prefix
                                (fill-common-string-prefix current-text text))
                               (prefix-length (length common-prefix)))
                          ;; Skip over common prefix of current text
                          ;; and new text.
                          (when (> prefix-length 0)
                            (goto-char (+ start prefix-length)))
                          (delete-region (point) end)
                          ;; Insert new text, minus common prefix.
                          (insert (substring text prefix-length)))))))
          (llm-chat-streaming provider prompt
                              (lambda (text) (insert-text text))
                              (lambda (text) (insert-text text)
                                (funcall finish-callback))
                              (lambda (_ msg) (error "Error calling the LLM: %s" msg))))))))

(cl-defmethod llm-chat-async ((_ (eql nil)) _ _ _)
  "Catch trivial configuration mistake."
  (error "LLM provider was nil.  Please set the provider in the application you are using"))

(cl-defmethod llm-chat-async :before (provider _ _ _)
  "Issue a warning if the LLM is non-free."
  (when-let (info (llm-nonfree-message-info provider))
    (llm--warn-on-nonfree (llm-name provider) info)))

(cl-defgeneric llm-capabilities (provider)
  "Return a list of the capabilities of PROVIDER.

This possible values are only those things that are not the bare
minimum of functionality to be included in this package, which is
non-streaming chat:

`streaming': the LLM can actually stream responses in the
streaming call.  Calls to `llm-chat-streaming' will work
regardless even if the LLM doesn't support streaming, it just
won't have any partial responses, so basically just operates like
`llm-chat-async'.

`embeddings': the LLM can return vector embeddings of text.

`function-calls': the LLM can call functions."
  (ignore provider)
  nil)

(cl-defgeneric llm-chat-token-limit (provider)
  "Return max number of tokens that can be sent to the LLM.
For many models we know this number, but for some we don't have
enough information to know.  In those cases we return a default
value that should be a reasonable lower bound.

PROVIDER is the provider struct that would be used for a LLM
call."
  (ignore provider)
  2048)

(cl-defgeneric llm-embedding (provider string)
  "Return a vector embedding of STRING from PROVIDER."
  (ignore provider string)
  (signal 'not-implemented nil))

(cl-defmethod llm-embedding ((_ (eql nil)) _)
  "Catch trivial configuration mistake."
  (error "LLM provider was nil.  Please set the provider in the application you are using"))

(cl-defmethod llm-embedding :before (provider _)
  "Issue a warning if the LLM is non-free."
  (when-let (info (llm-nonfree-message-info provider))
    (llm--warn-on-nonfree (llm-name provider) info)))

(cl-defgeneric llm-embedding-async (provider string vector-callback error-callback)
  "Calculate a vector embedding of STRING from PROVIDER.
VECTOR-CALLBACK will be called with the vector embedding.
ERROR-CALLBACK will be called in the event of an error, with an
error signal and a string message.

This returns an object representing the async request, which can
be passed to `llm-cancel-request'."
  (ignore provider string vector-callback error-callback)
  (signal 'not-implemented nil))

(cl-defmethod llm-embedding-async ((_ (eql nil)) _ _ _)
  "Catch trivial configuration mistake."
  (error "LLM provider was nil.  Please set the provider in the application you are using"))

(cl-defmethod llm-embedding-async :before (provider _ _ _)
  "Issue a warning if the LLM is non-free."
  (when-let (info (llm-nonfree-message-info provider))
    (llm--warn-on-nonfree (llm-name provider) info)))

(cl-defgeneric llm-count-tokens (provider string)
  "Return the number of tokens in STRING from PROVIDER.
This may be an estimate if the LLM does not provide an exact
count.  Different providers might tokenize things in different
ways."
  (ignore provider)
  (with-temp-buffer
    (insert string)
    (/ (* (count-words (point-min) (point-max)) 4) 3)))

(cl-defgeneric llm-cancel-request (request)
  "Cancel REQUEST, stopping any further communication.
REQUEST is the same object return by the async or streaming
methods."
  (ignore request)
  (lwarn 'llm :warning  "Canceling a request is not supported for this LLM."))

(cl-defmethod llm-cancel-request ((buf buffer))
  (cl-letf (((symbol-function 'url-http-async-sentinel) (lambda (_ _)))
            (kill-buffer-query-functions nil))
    (kill-buffer buf)))

(cl-defmethod llm-cancel-request ((proc process))
  (delete-process proc))

(cl-defgeneric llm-name (_)
  "Return the name of the model in PROVIDER.
This is expected to be suitable for short labels.  For example, if
the client wants to have a conversation with prefixes of `user> '
and a similar label for LLM (for example `Mistral> '), this
string should be short enough to fit that role.

Names are expected to be one word where possible, and
capitalized when appropriate.

This should be the name of the model, not the provider, where it
makes sense.  This is not expected to be unique per provider."
  "LLM")

(defun llm-chat-prompt-to-text (prompt)
  "Convert PROMPT `llm-chat-prompt' to a simple text.
This should only be used for logging or debugging."
  (concat
   (when (llm-chat-prompt-context prompt)
     (format "Context: %s\n" (llm-chat-prompt-context prompt)))
   (when (llm-chat-prompt-examples prompt)
     (concat "Examples:\n"
             (mapconcat (lambda (e) (format "  User: %s\n. Response: %s" (car e) (cdr e)))
                        (llm-chat-prompt-examples prompt) "\n")
             "\n"))
   "Interactions:\n"
   (mapconcat (lambda (i)
                (format "%s: %s"
                        (pcase (llm-chat-prompt-interaction-role i)
                          ('user "User")
                          ('system "System")
                          ('assistant "Assistant"))
                        (llm-chat-prompt-interaction-content i)))
              (llm-chat-prompt-interactions prompt) "\n")
   "\n"
   (when (llm-chat-prompt-temperature prompt)
     (format "Temperature: %s\n" (llm-chat-prompt-temperature prompt)))
   (when (llm-chat-prompt-max-tokens prompt)
     (format "Max tokens: %s\n" (llm-chat-prompt-max-tokens prompt)))))

(provide 'llm)
;;; llm.el ends here
