;;; llm-typesafe.el --- llm module for integrating with TypeSafe API -*- lexical-binding: t; package-lint-main-file: "llm.el"; byte-compile-docstring-max-column: 200-*-

;; Copyright (c) 2026  Free Software Foundation, Inc.

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
;; This file implements the llm decision functionality defined in llm.el, for
;; TypeSafe AI and compatible providers, such as Ollaya.

;;; Code:

(require 'llm)
(require 'llm-provider-utils)

(cl-defstruct (llm-typesafe-compatible
               (:include llm-standard-decide-provider)
               (:constructor make-llm-typesafe-compatible
                             (&key (model "unset")
                                   url
                                   ((:key raw-key))
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key)))))
  "A TypeSafe AI compatible provider, such as Ollaya.

URL is the URL for the provider's TypeSafe API entry point,
e.g. http://localhost:11435/v1/systemone."
  url model key)

(cl-defstruct (llm-typesafe
               (:include llm-typesafe-compatible
                         (url "https://api.typesafe.ai/v1/systemone"))
               (:constructor make-llm-typesafe
                             (&key (model "jev-latest")
                                   (url "https://api.typesafe.ai/v1/systemone")
                                   ((:key raw-key))
                                   &aux
                                   (key (llm-provider-utils--wrap-key raw-key)))))
  "A TypeSafe AI provider.")

(cl-defmethod llm-nonfree-message-info ((_ llm-typesafe))
  "Return TypeSafe AI nonfree terms of service."
  "https://typesafe.ai/legal/terms")

(cl-defmethod llm-provider-request-prelude ((provider llm-typesafe))
  (unless (llm-typesafe-key provider)
    (signal 'llm-provider-unconfigured '("To call the TypeSafe API, add a key to the `llm-typesafe' provider."))))

(cl-defmethod llm-provider-headers ((provider llm-typesafe-compatible))
  (when-let* ((key (llm-typesafe-compatible-key provider)))
    (when (functionp key)
      (setq key (funcall key)))
    `(("Authorization" . ,(format "Bearer %s" (encode-coding-string key 'utf-8))))))

(cl-defmethod llm-decide ((provider llm-typesafe-compatible) questions state)
  (llm-provider-utils-decide provider questions state))

(cl-defmethod llm-provider-decide-url ((provider llm-typesafe-compatible))
  (llm-typesafe-compatible-url provider))

(cl-defgeneric llm-typesafe-question-request (question)
  "Return a request alist for a question to be sent to the TypeSafe API.")

(cl-defmethod llm-typesafe-question-request ((question llm-question-bool))
  (cons (llm-question-bool-name question)
        (append
         (list :type "noul"
               :instructions (llm-question-bool-instructions question))
         (when (or (llm-question-bool-true-description question)
                   (llm-question-bool-false-description question))
           (list :criteria (append
                            (when (llm-question-bool-true-description question)
                              (list :true (llm-question-bool-true-description question)))
                            (when (llm-question-bool-false-description question)
                              (list :false (llm-question-bool-false-description question)))))))))

(cl-defmethod llm-typesafe-question-request ((question llm-question-choice))
  (cons (llm-question-choice-name question)
        (list :type "choice"
              :instructions (llm-question-choice-instructions question)

              :criteria (llm-question-choice-choices question))))

(cl-defmethod llm-typesafe-question-request ((question llm-question-score))
  (cons (llm-question-score-name question)
        (list :type "score"
              :instructions (llm-question-score-instructions question)
              :criteria (apply #'vector (llm-question-score-scale question)))))

(cl-defmethod llm-provider-decide-request ((provider llm-typesafe-compatible) questions state)
  (list
   :model (llm-typesafe-compatible-model provider)
   :questions (mapcar #'llm-typesafe-question-request questions)
   :state state))

(cl-defmethod llm-provider-decide-extract-error ((_ llm-typesafe-compatible) response)
  (assoc-default 'message (assoc-default 'error response)))

(cl-defmethod llm-provider-decide-extract-result ((_ llm-typesafe-compatible) response)
  (mapcar (lambda (answer)
            (cons
             (car answer)
             (let ((answer-data (cdr answer)))
               (pcase (assoc-default 'type answer-data)
                 ("noul" (make-llm-decision-bool :confidence (assoc-default 'noul answer-data)))
                 ("choice" (make-llm-decision-choice
                            :confidence (assoc-default 'confidence answer-data)
                            :choice (intern (assoc-default 'choice answer-data))
                            :probabilities
                            (assoc-default 'probabilities answer-data)))
                 ("score" (make-llm-decision-score
                           :confidence (assoc-default 'confidence answer-data)
                           :score (assoc-default 'score answer-data)
                           :probabilities
                           ;; Probabilities come in as index to value pairs
                           (mapcar #'cdr
                                   (sort (assoc-default 'probabilities answer-data)
                                         (lambda (a b)
                                           (< (string-to-number
                                               (symbol-name (car a)))
                                              (string-to-number
                                               (symbol-name (car b)))))))))))))
          (assoc-default 'answers response)))

(cl-defmethod llm-capabilities ((_ llm-typesafe-compatible))
  '(decision))

(provide 'llm-typesafe)

;;; llm-typesafe.el ends here
