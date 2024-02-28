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
(require 'seq)

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
      (if system-prompt
          (setf (llm-chat-prompt-interaction-content system-prompt)
                (concat (llm-chat-prompt-interaction-content system-prompt)
                        "\n"
                        system-content))
        (push (make-llm-chat-prompt-interaction
               :role 'system
               :content system-content)
              (llm-chat-prompt-interactions prompt))))))

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

(cl-defgeneric llm-provider-utils-populate-function-calls (provider prompt calls)
  "For PROVIDER, in PROMPT, record that function CALLS were received.
This is the recording before the calls were executed.
CALLS are a list of `llm-provider-utils-function-call'."
  (ignore provider prompt calls)
  (signal 'not-implemented nil))

(defun llm-provider-utils-populate-function-results (prompt func result)
  "Append the RESULT of FUNC to PROMPT.
FUNC is a `llm-provider-utils-function-call' struct."
  (llm-provider-utils-append-to-prompt
   prompt result (make-llm-chat-prompt-function-call-result
                  :call-id (llm-provider-utils-function-call-id func)
                  :function-name (llm-provider-utils-function-call-name func)
                  :result result)))

(defun llm-provider-utils-process-result (provider prompt response)
  "From RESPONSE, execute function call.

RESPONSE is either a string or list of
`llm-provider-utils-function-calls'.

This should be called with any response that might have function
calls. If the response is a string, nothing will happen, but in
either case, the response suitable for returning to the client
will be returned.

PROVIDER is the provider that supplied the response.

PROMPT was the prompt given to the provider, which will get
updated with the response from the LLM, and if there is a
function call, the result.

This returns the response suitable for output to the client; a
cons of functions called and their output."
  (if (consp response)
      (progn
        ;; Then this must be a function call, return the cons of a the funcion
        ;; called and the result.
        (llm-provider-utils-populate-function-calls provider prompt response)
        (cl-loop for func in response collect
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
                                   prompt func result)
                                  (llm--log
                                   'api-funcall
                                   :provider provider
                                   :msg (format "%s --> %s"
                                                (format "%S"
                                                        (cons (llm-function-call-name function)
                                                              args))
                                                (format "%s" result)))
                                  result)))))
    (llm-provider-utils-append-to-prompt prompt response)
    response))

(provide 'llm-provider-utils)
;;; llm-provider-utils.el ends here
