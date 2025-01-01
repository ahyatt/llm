;;; elisp-to-function-call --- Utility for converting elisp to function call -*- lexical-binding: t; -*-

;; Copyright (c) 2024  Free Software Foundation, Inc.

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
;; This is a utility class for clients of the llm library. It is used to make
;; writing function calls for existing elisp code automated, through the use of
;; function calling. We use a function call to take in a elisp function,
;; something with documentation, and we add a function call to where the
;; function was called from.

(require 'llm)
(require 'rx)
(require 'cl-extra)

(defvar elisp-to-function-call-provider nil
  "The LLM provider to use for this.  Must support function calls.")

;; An example of the output - you can remove the function call definition and
;; call `elisp-to-function-call-insert' to see this in action.
(defconst elisp-to-function-call-switch-to-buffer 
  (make-llm-function-call :function
                          'switch-to-buffer
                          :name
                          "switch_to_buffer"
                          :args
                          (list (make-llm-function-arg :name
                                                       "buffer_or_name"
                                                       :type
                                                       'string
                                                       :description
                                                       "A buffer, a string (buffer name), or nil."
                                                       :required
                                                       t)
                                (make-llm-function-arg :name
                                                       "norecord"
                                                       :type
                                                       'boolean
                                                       :description
                                                       "If non-nil, do not put the buffer at the front of the buffer list and do not make the window displaying it the most recently selected one."
                                                       :required
                                                       t)
                                (make-llm-function-arg :name
                                                       "force_same_window"
                                                       :type
                                                       'boolean
                                                       :description
                                                       "If non-nil, the buffer must be displayed in the selected window when called non-interactively; if that is impossible, signal an error rather than calling ‘pop_to_buffer’."
                                                       :required
                                                       t))
                          :description
                          "Display buffer BUFFER_OR_NAME in the selected window. If the selected window cannot display the specified buffer because it is a minibuffer window or strongly dedicated to another buffer, call ‘pop_to_buffer’ to select the buffer in another window. If called interactively, read the buffer name using ‘read_buffer’. The variable ‘confirm_nonexistent_file_or_buffer’ determines whether to request confirmation before creating a new buffer. See ‘read_buffer’ for features related to input and completion of buffer names. BUFFER_OR_NAME may be a buffer, a string (a buffer name), or nil. If BUFFER_OR_NAME is a string that does not identify an existing buffer, create a buffer with that name. If BUFFER_OR_NAME is nil, switch to the buffer returned by ‘other_buffer’. Return the buffer switched to.")
  
  )

;; A demonstration of the resulting function call in action.
(defun elisp-to-function-call-llm-switch-buffer (instructions)
  "Send INSTRUCTIONS to the LLM so that it siwtches the buffer.
It will call `elisp-to-function-call-provider.', and will pass
the available buffers in the prompt."
  (interactive "sInstructions: ")
  (llm-chat-async elisp-to-function-call-provider
                  (make-llm-chat-prompt
                   :context (format "The user wishes to switch to a buffer.  The available buffers to switch to are: %s.  Please call the switch_to_buffer function and make your best guess at what which of the buffers the user wants, or a new buffer if that is appropriate."
                                    (json-encode
                                     (seq-filter (lambda (s) (not (string-match "^\s" s)))
                                                 (mapcar #'buffer-name (buffer-list)))))
                   :interactions (list (make-llm-chat-prompt-interaction
                                        :role 'user
                                        :content instructions))
                   :functions (list elisp-to-function-call-switch-to-buffer))
                  (lambda (_))  ;; Nothing to do, the switch already happened.
                  (lambda (_ msg) (error msg))))

(defun elisp-to-function-call-el-to-js-name (name)
  "Convert NAME to a JavaScript name."
  (replace-regexp-in-string (rx (seq (group-n 1 alpha) ?- (group-n 2 alpha)))
                            "\\1_\\2" name))

(defun elisp-to-function-call-insert (f)
  "For non-anonymous function F, insert a function spec for LLMs.
The definition is for a `llm-function-call'.

What comes out should be close to correct, but it may need some
manual intervention.

The function spec here makes Gemini error out, perhaps because it
uses more nested function specs. This may go away eventually as
Gemini improves."
  (interactive "aFunction: ")
  (let ((marker (point-marker))
        (arglist (help-function-arglist f)))
    (llm-chat-async elisp-to-function-call-provider
                    (make-llm-chat-prompt
                     :context "The user wants to get the data to transform an emacs lisp
function to a function usable in a OpenAI-compatible function call.  The user will
provide the function name and documentation.  Break that down into the documentation
of the function, and the argument types and descriptions for those arguments.

Use lowercase for all argument names even if you see it in uppercase in the documentation.
Documentation strings should start with uppercase and end with a period."
                     :interactions (list (make-llm-chat-prompt-interaction
                                          :role 'user
                                          :content (format "Function: %s\nArguments: %s\nDescription: %s"
                                                           (elisp-to-function-call-el-to-js-name (symbol-name f))
                                                           (if arglist
                                                               (format "%s" arglist)
                                                             "No arguments")
                                                           (elisp-to-function-call-el-to-js-name
                                                            (documentation f)))))
                     :functions
                     (list
                      (make-llm-function-call
                       :function (lambda (args description)
                                   (with-current-buffer (marker-buffer marker)
                                     (save-excursion
                                       (goto-char marker)
                                       (cl-prettyprint
                                        `(make-llm-function-call
                                          :function ,(list 'quote f)
                                          :name ,(elisp-to-function-call-el-to-js-name (symbol-name f))
                                          :args (list
                                                 ,@(mapcar (lambda (arg)
                                                             `(make-llm-function-arg
                                                               ,@(append
                                                                  (list
                                                                   :name (downcase (elisp-to-function-call-el-to-js-name
                                                                                    (assoc-default 'name arg)))
                                                                   :type (list 'quote (read (assoc-default 'type arg)))
                                                                   :description (assoc-default 'description arg))
                                                                  (if (assoc-default 'required arg)
                                                                      (list :required t)))))
                                                           args))
                                          :description ,description)))))
                       :name "elisp-to-function-info"
                       :description "The function to create a OpenAI-compatible function call spec, given the arguments and their documentation.  Some of the aspects of the function call can be automatically retrieved, so this function is supplying the parts that cannot be automatically retrieved."
                       :args (list
                              (make-llm-function-arg
                               :name "args"
                               :type `(list
                                       ,(make-llm-function-arg
                                         :name "name"
                                         :type 'string
                                         :description "The name of the argument"
                                         :required t)
                                       ,(make-llm-function-arg
                                         :name "type"
                                         :type '(enum string number integer boolean "(list string)" "(list integer)" "(list number)")
                                         :description "The type of the argument.  It could be 'string', 'number', 'integer', 'boolean', or the more special forms.
(list string) is for a list of strings, (list integer), etc."
                                         :required t)
                                       ,(make-llm-function-arg
                                         :name "description"
                                         :type 'string
                                         :description "The description of the argument"
                                         :required t)
                                       ,(make-llm-function-arg
                                         :name "required"
                                         :type 'boolean
                                         :description "Whether the argument is required or not"
                                         :required t))
                               :description "The arguments of the function to transform, in order.")
                              (make-llm-function-arg
                               :name "description"
                               :type 'string
                               :description "The documentation of the function to transform.")))))
                    (lambda (result) (message "Result: %S" result))
                    (lambda (_ msg) (error msg)))))

(provide 'elisp-to-function-call)
