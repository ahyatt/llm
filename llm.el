;;; llm.el --- Interface to pluggable llm backends -*- lexical-binding: t -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm
;; Package-Requires: ((emacs "28.1"))
;; Package-Version: 0.7.0
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

CONTEXT is a string given to the LLM as context for the entire
interaction, such as instructions to the LLM on how to reply,
persona, information on the user, or anything else that applies
to the chat as a whole.  This is optional.

EXAMPLES is a list of conses, where the car is an example
inputs, and cdr is the corresponding example outputs.  This is optional.

INTERACTIONS is a list message sent by either the llm or the
user. It is a either list of `llm-chat-prompt-interaction'
objects or list of an opaque converation ID (anything not a
`llm-chat-prompt-interaction') and the latest
`llm-chat-prompt-interaction' in the conversation to submit. When
building up a chat, the chat methods update this to a new value,
and the client is expected to append a new interaction to the
end, without introspecting the value otherwise. The function
`llm-chat-prompt-append-response' accomplishes that operation, and
should be used. 'Because this value updated by the called
function, for continuing chats, the whole prompt MUST be a
variable passed in to the chat function. INTERACTIONS is
required.

TEMPERATURE is a floating point number with a minimum of 0, and
maximum of 1, which controls how predictable the result is, with
0 being the most predicatable, and 1 being the most creative.
This is not required.

MAX-TOKENS is the maximum number of tokens to generate.  This is optional."
  context examples interactions temperature max-tokens)

(cl-defstruct llm-chat-prompt-interaction
  "This defines a single interaction given as part of a chat prompt.
ROLE can a symbol, of either `user' or `assistant'."
  role content)

(defun llm-make-simple-chat-prompt (text)
  "Create a `llm-chat-prompt' with TEXT sent to the LLM provider.
This is a helper function for when you just need to send text to
an LLM, and don't need the more advanced features that the
`llm-chat-prompt' struct makes available."
  (make-llm-chat-prompt :interactions (list (make-llm-chat-prompt-interaction :role 'user :content text))))

(defun llm-chat-prompt-append-response (prompt response &optional role)
  "Append a new RESPONSE to PROMPT, to continue a conversation.
ROLE default to `user', which should almost always be what is needed."
  (setf (llm-chat-prompt-interactions prompt)
        (append (llm-chat-prompt-interactions prompt)
                (list (make-llm-chat-prompt-interaction :role (or role 'user)
                                                        :content response)))))

(cl-defgeneric llm-nonfree-message-info (provider)
  "If PROVIDER is non-free, return info for a warning.
This should be a cons of the name of the LLM, and the URL of the
terms of service.

If the LLM is free and has no restrictions on use, this should
return nil.  Since this function already returns nil, there is no
need to override it."
  (ignore provider)
  nil)

(cl-defgeneric llm-chat (provider prompt)
  "Return a response to PROMPT from PROVIDER.
PROMPT is a `llm-chat-prompt'. The response is a string response by the LLM.

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
    (llm--warn-on-nonfree (car info) (cdr info))))

(cl-defgeneric llm-chat-async (provider prompt response-callback error-callback)
  "Return a response to PROMPT from PROVIDER.
PROMPT is a `llm-chat-prompt'.

RESPONSE-CALLBACK receives the final text.

ERROR-CALLBACK receives the error response.

The prompt's interactions list will be updated to encode the
conversation so far.

This returns an object representing the async request, which can
be passed to `llm-cancel-request'."
  ;; By default, you can turn a streaming call into an async call, so we can
  ;; fall back to streaming if async is not populated.
  (llm-chat-streaming provider prompt
                      ;; Do nothing on partial callback
                      (lambda (_))
                      (lambda (text)
                        (funcall response-callback text))
                      (lambda (err msg) (funcall error-callback err msg))))

(cl-defgeneric llm-chat-streaming (provider prompt partial-callback response-callback error-callback)
  "Stream a response to PROMPT from PROVIDER.
PROMPT is a `llm-chat-prompt'.

PARTIAL-CALLBACK is called with the output of the string response
as it is built up. The callback is called with the entire
response that has been received, as it is streamed back. It is
not guaranteed to be called with the complete response before
RESPONSE-CALLBACK is called.

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
    (llm--warn-on-nonfree (car info) (cdr info))))

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
                        (delete-region start end)
                        (insert text)))))
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
    (llm--warn-on-nonfree (car info) (cdr info))))

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
    (llm--warn-on-nonfree (car info) (cdr info))))

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
    (llm--warn-on-nonfree (car info) (cdr info))))

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

(defun llm-chat-prompt-to-text (prompt)
  "Convert PROMPT `llm-chat-prompt' to a simple text.
This should only be used for logging or debugging."
  (format "Context: %s\nExamples: %s\nInteractions: %s\n%s%s\n"
          (llm-chat-prompt-context prompt)
          (mapconcat (lambda (e) (format "User: %s\nResponse: %s" (car e) (cdr e)))
                     (llm-chat-prompt-examples prompt) "\n")
          (mapconcat (lambda (i)
               (format "%s: %s"
                       (pcase (llm-chat-prompt-interaction-role i)
                         ('user "User")
                         ('system "System")
                         ('assistant "Assistant"))
                       (llm-chat-prompt-interaction-content i)))
                     (llm-chat-prompt-interactions prompt) "\n")
          (if (llm-chat-prompt-temperature prompt)
              (format "Temperature: %s\n" (llm-chat-prompt-temperature prompt))
            "")
          (if (llm-chat-prompt-max-tokens prompt)
              (format "Max tokens: %s\n" (llm-chat-prompt-max-tokens prompt))
            "")))

(provide 'llm)
;;; llm.el ends here
