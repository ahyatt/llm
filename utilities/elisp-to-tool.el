;;; elisp-to-tool --- Utility for converting elisp to function call -*- lexical-binding: t; -*-

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
;; This is a utility class for clients of the llm library. It is used to make
;; writing function calls for existing elisp code automated, through the use of
;; function calling. We use a function call to take in a elisp function,
;; something with documentation, and we add a function call to where the
;; function was called from.

(require 'llm)
(require 'rx)
(require 'cl-extra)

(defvar elisp-to-tool-provider nil
  "The LLM provider to use for this.  Must support tool use.")

;; An example of the output - you can remove the function call definition and
;; call `elisp-to-tool-insert' to see this in action.
(defconst elisp-to-tool-switch-to-buffer
  (llm-make-tool :function
                 'switch-to-buffer
                 :name
                 "switch_to_buffer"
                 :args
                 '((:name "buffer_or_name" :type string :description "A buffer, a string (buffer name), or nil. If a string that doesn't identify an existing buffer, create a buffer with that name. If nil, switch to the buffer returned by 'other_buffer'." :required t) (:name "norecord" :type boolean :description "If non-nil, do not put the buffer at the front of the buffer list, and do not make the window displaying it the most recently selected one." :required t) (:name "force_same_window" :type boolean :description "If non-nil, the buffer must be displayed in the selected window when called non-interactively; if impossible, signal an error rather than calling 'pop_to_buffer'." :required t))
                 :description
                 "Display buffer BUFFER_OR_NAME in the selected window. WARNING: This is NOT the way to work on another buffer temporarily within a Lisp program! Use 'set_buffer' instead. That avoids messing with the 'window_buffer' correspondences. If the selected window cannot display the specified buffer because it is a minibuffer window or strongly dedicated to another buffer, call 'pop_to_buffer' to select the buffer in another window. In interactive use, if the selected window is strongly dedicated to its buffer, the value of the option 'switch_to_buffer_in_dedicated_window' specifies how to proceed. Return the buffer switched to."
                 :async
                 nil)
  )

;; A demonstration of the resulting function call in action.
(defun elisp-to-tool-llm-switch-buffer (instructions)
  "Send INSTRUCTIONS to the LLM so that it siwtches the buffer.
It will call `elisp-to-tool-provider.', and will pass
the available buffers in the prompt."
  (interactive "sInstructions: ")
  (llm-chat-async elisp-to-tool-provider
                  (llm-make-chat-prompt
                   instructions
                   :context (format "The user wishes to switch to a buffer.  The available buffers to switch to are: %s.  Please call the switch_to_buffer function and make your best guess at what which of the buffers the user wants, or a new buffer if that is appropriate."
                                    (format "%s"
                                            (vconcat (seq-filter (lambda (s) (not (string-match "^\s" s)))
                                                                 (mapcar #'buffer-name (buffer-list))))))
                   :tools (list elisp-to-tool-switch-to-buffer))
                  (lambda (_))  ;; Nothing to do, the switch already happened.
                  (lambda (_ msg) (error msg))))

(defun elisp-to-tool-el-to-js-name (name)
  "Convert NAME to a JavaScript name."
  (replace-regexp-in-string (rx (seq (group-n 1 alpha) ?- (group-n 2 alpha)))
                            "\\1_\\2" name))

(defun elisp-to-tool-insert (f)
  "For non-anonymous function F, insert a function spec for LLMs.
The definition is for a `llm-tool-function'.

What comes out should be close to correct, but it may need some
manual intervention.

The function spec here makes Gemini error out, perhaps because it
uses more nested function specs. This may go away eventually as
Gemini improves."
  (interactive "aFunction: ")
  (let ((marker (point-marker))
        (arglist (help-function-arglist f)))
    (llm-chat-async elisp-to-tool-provider
                    (llm-make-chat-prompt
                     (format "Function: %s\nArguments: %s\nDescription: %s"
                             (elisp-to-tool-el-to-js-name (symbol-name f))
                             (if arglist
                                 (format "%s" arglist)
                               "No arguments")
                             (elisp-to-tool-el-to-js-name
                              (documentation f)))
                     :context "The user wants to get the data to transform an emacs lisp
function to a function usable in a OpenAI-compatible function call.  The user will
provide the function name and documentation.  Break that down into the documentation
of the function, and the argument types and descriptions for those arguments.

Use lowercase for all argument names even if you see it in uppercase in the documentation.
Documentation strings should start with uppercase and end with a period."
                     :tools
                     (list
                      (llm-make-tool
                       :function
                       (lambda (args description)
                         (with-current-buffer (marker-buffer marker)
                           (save-excursion
                             (goto-char marker)
                             (cl-prettyprint
                              `(llm-make-tool
                                :function ,(list 'quote f)
                                :name ,(elisp-to-tool-el-to-js-name (symbol-name f))
                                :args '(,@(mapcar
                                           (lambda (arg)
                                             (append
                                              (list
                                               :name (downcase (elisp-to-tool-el-to-js-name
                                                                (assoc-default 'name arg)))
                                               :type (intern (assoc-default 'type arg))
                                               :description (assoc-default 'description arg))
                                              (if (assoc-default 'required arg)
                                                  (list :required t))))
                                           args))
                                :description ,description
                                :async nil)))))
                       :name "elisp-to-tool-info"
                       :description "The function to create a OpenAI-compatible tool use spec, given the arguments and their documentation.  Some of the aspects of the tool can be automatically retrieved, so this function is supplying the parts that cannot be automatically retrieved."
                       :args '((:name "args"
                                      :type array
                                      :items (:type object
                                                    :properties (:name
                                                                 (:type string
                                                                        :description "The name of the argument")
                                                                 :type
                                                                 (:type string
                                                                        :enum ["string""number" "integer" "boolean"]
                                                                        :description "The type of the argument.  It could be 'string', 'number', 'integer', 'boolean', or the more special forms.")
                                                                 :description
                                                                 (:type string
                                                                        :description "The description of the argument")
                                                                 :required
                                                                 (:type boolean
                                                                        :description "Whether the argument is required or not"))))
                               (:name "description"
                                      :type string
                                      :description "The documentation of the function to transform.")))))
                    (lambda (result) (message "Result: %S" result))
                    (lambda (_ msg) (error msg)))))

(provide 'elisp-to-tool)
