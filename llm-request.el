;;; llm-request.el --- Request handling code -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
(require 'cl-macs)
(require 'url-http)
(require 'rx)

(defcustom llm-request-timeout 60
  "The number of seconds to wait for a response from a HTTP server.

Request timings are depending on the request. Requests that need
more output may take more time, and there is other processing
besides just token generation that can take a while. Sometimes
the LLM can get stuck, and you don't want it to take too long.
This should be balanced to be good enough for hard requests but
not very long so that we can end stuck requests."
  :type 'integer
  :group 'llm)

(defun llm-request--content ()
  "From the current buffer, return the content of the response."
  (decode-coding-string
   (buffer-substring-no-properties
    (or (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
        (save-match-data
          (save-excursion
            (goto-char (point-min))
            (search-forward "\n\n" nil t)
            (forward-line)
            (point))))
    (point-max)) 'utf-8))

(defvar-local llm-request--partial-callback nil
  "The callback to call when a partial response is received.")

(defvar url-http-response-status)

(cl-defun llm-request-sync-raw-output (url &key headers data timeout)
  "Make a request to URL.  The raw text response will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

TIMEOUT is the number of seconds to wait for a response."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (append headers '(("Content-Type" . "application/json"))))
        (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (let ((buf (url-retrieve-synchronously url t nil (or timeout llm-request-timeout))))
      (if buf
          (with-current-buffer buf
            (url-http-parse-response)
            (if (and (>= url-http-response-status 200)
                     (< url-http-response-status 300))
                (llm-request--content)
                (error "LLM request failed with code %d: %s (additional information: %s)"
                       url-http-response-status
                       (nth 2 (assq url-http-response-status url-http-codes))
                       (string-trim (llm-request--content)))))
        (error "LLM request timed out")))))

(cl-defun llm-request-sync (url &key headers data timeout)
  "Make a request to URL.  The parsed response will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

TIMEOUT is the number of seconds to wait for a response."
  (json-read-from-string (llm-request-sync-raw-output url
                                                      :headers headers
                                                      :data data
                                                      :timeout timeout)))

(defun llm-request--handle-new-content (_ _ pre-change)
  "Handle new content in the current buffer.
PRE-CHANGE is the length of text replaced, which for insertions
is zero."
  (when (= 0 pre-change)
    (save-match-data
      (save-excursion
        ;; Make sure we actually have any content before invoking a callback.
        (when (and llm-request--partial-callback
                   (boundp 'url-http-end-of-headers)
                   url-http-end-of-headers)
          (funcall llm-request--partial-callback (llm-request--content)))))))

(cl-defun llm-request-async (url &key headers data on-success on-success-raw on-error on-partial)
  "Make a request to URL.
Nothing will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

ON-SUCCESS will be called with the response body as a json
object. This is optional in the case that ON-SUCCESS-DATA is set,
and required otherwise.

ON-ERROR will be called with the error code and a response-body.
This is required.

ON-PARTIAL will be called with the potentially incomplete response
body as a string.  This is an optional argument.

ON-SUCCESS-RAW, if set, will be called in the buffer with the
response body, and expect the response content. This is an
optional argument, and mostly useful for streaming.  If not set,
the buffer is turned into JSON and passed to ON-SUCCESS."
  (let ((url-request-method "POST")
        ;; This is necessary for streaming, otherwise we get gzip'd data that is
        ;; unparseable until the end. The responses should be small enough that
        ;; this should not be any big loss.
        (url-mime-encoding-string "identity")
        (url-request-extra-headers
         (append headers '(("Content-Type" . "application/json"))))
        (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (let ((buffer
           (url-retrieve
            url
            ;; For some reason the closure you'd expect did not work here.
            (lambda (_ on-success on-error)
              ;; No matter what, we need to stop listening for changes.
              (remove-hook 'after-change-functions #'llm-request--handle-new-content t)
              (let ((code (url-http-parse-response)))
                (if (eq code 200)
                    (if on-success-raw
                        (funcall on-success-raw (llm-request--content))
                      (funcall on-success (json-read-from-string (llm-request--content))))
                  (funcall on-error code (ignore-errors
                                           (json-read-from-string (llm-request--content)))))))
            (list on-success on-error)
            t)))
      (when (and buffer on-partial)
        (with-current-buffer buffer
          (setq llm-request--partial-callback on-partial)
          (add-hook 'after-change-functions
                    #'llm-request--handle-new-content
                    nil t)))
      buffer)))

;; This is a useful method for getting out of the request buffer when it's time
;; to make callbacks.
(defun llm-request-callback-in-buffer (buf f &rest args)
  "Run F with ARSG in the context of BUF.
But if BUF has been killed, use a temporary buffer instead.
If F is nil, nothing is done."
  (when f
    (if (buffer-live-p buf)
        (with-current-buffer buf (apply f args))
      (with-temp-buffer (apply f args)))))

(provide 'llm-request)
;;; llm-request.el ends here
