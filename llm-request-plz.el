;;; llm-request-plz.el --- Curl request handling code -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

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
(require 'cl-macs)
(require 'json)
(require 'plz-event-source)
(require 'plz-media-type)
(require 'rx)
(require 'url-http)

(defcustom llm-request-plz-timeout 60
  "The number of seconds to wait for a response from a HTTP server.

Request timings are depending on the request. Requests that need
more output may take more time, and there is other processing
besides just token generation that can take a while. Sometimes
the LLM can get stuck, and you don't want it to take too long.
This should be balanced to be good enough for hard requests but
not very long so that we can end stuck requests."
  :type 'integer
  :group 'llm)

(defun llm-request-success (status)
  "Return non-nil if STATUS is a successful HTTP status code."
  (<= 200 status 299))

(cl-defun llm-request-plz-sync-raw-output (url &key headers data timeout)
  "Make a request to URL.  The raw text response will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

TIMEOUT is the number of seconds to wait for a response."
  (condition-case error
      (let ((resp (plz-media-type-request
        'post url
        :as `(media-types ,plz-media-types)
        :body (when data
                (encode-coding-string (json-encode data) 'utf-8))
        :headers (append headers '(("Content-Type" . "application/json")))
        :timeout (or timeout llm-request-plz-timeout))))
        (if (llm-request-success (plz-response-status resp))
            (plz-response-body resp)
          (signal 'plz-http-error resp)))
    (plz-error
     (seq-let [error-sym message data] error
       (cond
        ((eq 'plz-http-error error-sym)
         (let ((response (plz-error-response data)))
           (error "LLM request failed with code %d: %s (additional information: %s)"
                  (plz-response-status response)
                  (nth 2 (assq (plz-response-status response) url-http-codes))
                  (plz-response-body response))))
        ((and (eq 'plz-curl-error error-sym)
              (eq 28 (car (plz-error-curl-error data))))
         (error "LLM request timed out"))
        (t (signal error-sym (list message data))))))))

(cl-defun llm-request-plz-sync (url &key headers data timeout)
  "Make a request to URL.  The parsed response will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

TIMEOUT is the number of seconds to wait for a response."
  (llm-request-plz-sync-raw-output url
                                   :headers headers
                                   :data data
                                   :timeout timeout))

(defun llm-request-plz--handle-error (error on-error)
  "Handle the ERROR with the ON-ERROR callback.

For HTTP errors, ON-ERROR will be called with the HTTP status
code and the HTTP body of the error response.

For Curl errors, ON-ERROR will be called with the exit code of
the curl process and an error message."
  (cond ((plz-error-response error)
         (let ((response (plz-error-response error)))
           (funcall on-error
                    (plz-response-status response)
                    (plz-response-body response))))
        ((plz-error-curl-error error)
         (let ((curl-error (plz-error-curl-error error)))
           (funcall on-error
                    (car curl-error)
                    (cdr curl-error))))
        (t (user-error "Unexpected error: %s" error))))

(cl-defun llm-request-plz-async (url &key headers data on-success on-success-raw on-error
                                     on-partial timeout)
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
  (plz-media-type-request
    'post url
    :as 'string
    :body (when data
            (encode-coding-string (json-encode data) 'utf-8))
    :headers (append headers
                     '(("Accept-encoding" . "identity")
                       ("Content-Type" . "application/json")))
    :then (lambda (response)
            (when on-success-raw
              (funcall on-success-raw response))
            (when on-success
              (funcall on-success (json-read-from-string response))))
    :else (lambda (error)
            (when on-error
              (llm-request-plz--handle-error error on-error)))
    :timeout (or timeout llm-request-plz-timeout)))

(cl-defun llm-request-plz-event-stream (url &key headers data on-error on-success
                                            event-stream-handlers timeout)
  "Make a request to URL.
Nothing will be returned.

HEADERS will be added in the Authorization header, in addition to
standard json header. This is optional.

DATA will be jsonified and sent as the request body.
This is required.

ON-SUCCESS will be called with the response body as a json
object. This is optional in the case that ON-SUCCESS-DATA is set,
and required otherwise.

EVENT-STREAM-HANDLERS are an alist of event names to functions
that handle the event's corresponding data, which will be called
with the new event data as a string.

ON-ERROR will be called with the error code and a response-body.
This is required.
"
  (plz-media-type-request
    'post url
    :as `(media-types
          ,(cons
            (cons "text/event-stream"
                  (plz-media-type:text/event-stream
                   ;; Convert so that each event handler gets the body, not the
                   ;; `plz-response' itself.
                   :events (mapcar
                            (lambda (cons)
                              (cons (car cons)
                                    (lambda (_ resp) (funcall (cdr cons) (plz-event-source-event-data resp)))))
                            event-stream-handlers)))
            plz-media-types))
    :body (when data
            (encode-coding-string (json-encode data) 'utf-8))
    :headers (append headers
                     '(("Accept-encoding" . "identity")
                       ("Content-Type" . "application/json")))
    :then (lambda (response)
            (when on-success
              (funcall on-success (plz-response-body response))))
    :else (lambda (error)
            (when on-error
              (llm-request-plz--handle-error error on-error)))
    :timeout (or timeout llm-request-plz-timeout)))

;; This is a useful method for getting out of the request buffer when it's time
;; to make callbacks.
(defun llm-request-plz-callback-in-buffer (buf f &rest args)
  "Run F with ARSG in the context of BUF.
But if BUF has been killed, use a temporary buffer instead.
If F is nil, nothing is done."
  (when f
    (if (buffer-live-p buf)
        (with-current-buffer buf (apply f args))
      (with-temp-buffer (apply f args)))))

(provide 'llm-request-plz)
;;; llm-request-plz.el ends here
