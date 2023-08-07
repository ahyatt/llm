;;; llm.el --- Interface to pluggable llm backends -*- lexical-binding: t -*-

;; Copyright (c) 2023  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm
;; Package-Requires: ((request "0.3.3") (emacs "28.1"))
;; Package-Version: 0.1
;; Keywords: outlines, hypermedia
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
;; functionality they can provide. Not all LLMs will support all of these, but
;; programs that want to integrate with LLMs can code against the interface, and
;; users can then choose the LLMs they want to use. It's advisable to have the
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

(cl-defstruct llm-chat-prompt
  "This stores all the information needed for a structured chat prompt.

CONTEXT is a string given to the LLM as context for the entire
interaction, such as instructions to the LLM on how to reply,
persona, information on the user, or anything else that applies
to the chat as a whole.  This is optional.

EXAMPLES is a list of conses, where the car is an example
inputs, and cdr is the corresponding example outputs.  This is optional.

INTERACTIONS is a list message sent by either the llm or the
user. It is a list of `llm-chat-prompt-interaction' objects. This
is required.

TEMPERATURE is a floating point number with a minimum of 0, and
maximum of 1, which controls how predictable the result is, with
0 being the most predicatable, and 1 being the most creative.
This is not required.

MAX-TOKENS is the maximum number of tokens to generate.  This is optional.
"
  context examples interactions temperature max-tokens)

(cl-defstruct llm-chat-prompt-interaction
  "This defines a single interaction given as part of a chat prompt.
ROLE can a symbol, of either `user' or `assistant'."
  role content)

(cl-defgeneric llm-chat-response (provider prompt)
  "Return a response to PROMPT from PROVIDER.
PROMPT is a `llm-chat-prompt'. The response is a string."
  (ignore provider prompt)
  (signal 'not-implemented nil))

(cl-defgeneric llm-embedding (provider string)
  "Return a vector embedding of STRING from PROVIDER."
  (ignore provider string)
  (signal 'not-implemented nil))

(cl-defgeneric llm-count-tokens (provider string)
  "Return the number of tokens in STRING from PROVIDER.
This may be an estimate if the LLM does not provide an exact
count. Different providers might tokenize things in different
ways."
    (ignore provider)
    (with-temp-buffer
      (insert string)
      (/ (* (count-words (point-min) (point-max)) 4) 3)))

(defun llm-chat-prompt-to-text (prompt)
  "Convert PROMPT `llm-chat-prompt' to a simple text.
This should only be used for logging or debugging."
  (format "Context: %s\nExamples: %s\nInteractions: %s\nTemperature: %f\nMax tokens: %d\n"
          (llm-chat-prompt-context prompt)
          (mapconcat (lambda (e) (format "User: %s\nResponse: %s" (car e) (cdr e)))
                     (llm-chat-prompt-interactions prompt) "\n")
          (mapconcat (lambda (i)
               (format "%s: %s"
                       (pcase (llm-chat-prompt-interaction-role i)
                         ('user "User")
                         ('system "System")
                         ('assistant "Assistant"))
                       (llm-chat-prompt-interaction-content i)))
                     (llm-chat-prompt-interactions prompt) "\n")
          (llm-chat-prompt-temperature prompt)
          (llm-chat-prompt-max-tokens prompt)))

(provide 'llm)

;;; llm.el ends here
