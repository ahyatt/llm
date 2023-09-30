;;; llm-request.el --- Request handling code -*- lexical-binding: t -*-

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
;; This file provides basic functions for providers who need to request data. It
;; assumes the server is using json.

;;; Code:
(require 'json)
(require 'url-http)
(require 'rx)

(defun llm-request--content ()
  "From the current buffer, return the content of the response."
  (goto-char (point-min))
    (re-search-forward (rx (seq line-start
                                (zero-or-one control)
                                line-end)))
    (forward-line)
    (buffer-substring-no-properties (point) (point-max)))

(cl-defun llm-request-sync (url &key headers data timeout)
  "Make a request to URL.  The parsed response will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

TIMEOUT is the number of seconds to wait for a response."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (append headers '(("Content-Type" . "application/json"))))
        (url-request-data (json-encode data)))
    (let ((buf (url-retrieve-synchronously url t nil (or timeout 5))))
      (if buf
          (with-current-buffer buf
            (json-read-from-string (llm-request--content)))
        (error "LLM request timed out")))))

(cl-defun llm-request-async (url &key headers data on-success on-error on-chunk)
  "Make a request to URL.
Nothing will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

ON-SUCCESS will be called with the response body as a json object.
This is required.

ON-ERROR will be called with the error code and a response-body.
This is required.

ON-CHUNK will be called with the potentially incomplete response
body as a string.  This is an optional argument."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (append headers '(("Content-Type" . "application/json"))))
        (url-request-data (json-encode data)))
    (let ((buffer
           (url-retrieve
            url
            ;; For some reason the closure you'd expect did not work here.
            (lambda (_ on-success on-error)
              (let ((code (url-http-parse-response)))
                (if (eq code 200)
                    (funcall on-success (json-read-from-string (llm-request--content)))
                  (funcall on-error code (ignore-errors
                                           (json-read-from-string (llm-request--content)))))))
            (list on-success on-error)
            t)))
      (when on-chunk
        (with-current-buffer buffer
          (add-hook 'after-change-functions
                    (lambda (_ _ _)
                      (funcall on-chunk (llm-request--content)))))))))

(provide 'llm-request)
;;; llm-request.el ends here
