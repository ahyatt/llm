;;; plz-media-type.el --- plz content types -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>

;; This file is part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file handles content type.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'eieio)
(require 'plz)

(defclass plz-media-type ()
  ((type
    :documentation "The media type."
    :initarg :type
    :type symbol)
   (subtype
    :documentation "The media subtype."
    :initarg :subtype
    :subtype symbol)
   (parameters
    :documentation "The parameters of the media type."
    :initarg :parameters
    :initform nil
    :subtype list)))

(defun plz-media-type-charset (media-type)
  "Return the character set of the MEDIA-TYPE."
  (with-slots (parameters) media-type
    (alist-get "charset" parameters nil nil #'equal)))

(defun plz-media-type-coding-system (media-type)
  "Return the coding system of the MEDIA-TYPE."
  (coding-system-from-name (or (plz-media-type-charset media-type) "UTF-8")))

(defun plz-media-type-name (media-type)
  "Return the name of the MEDIA-TYPE as a string."
  (with-slots (type subtype) media-type
    (format "%s/%s" type subtype)))

(defun plz-media-type-symbol (media-type)
  "Return the name of the MEDIA-TYPE as a symbol."
  (intern (plz-media-type-name media-type)))

(cl-defgeneric plz-media-type-else (media-type error)
  "Transform the ERROR into a format suitable for MEDIA-TYPE.")

(cl-defgeneric plz-media-type-then (media-type response)
  "Transform the RESPONSE into a format suitable for MEDIA-TYPE.")

(cl-defgeneric plz-media-type-process (media-type process chunk)
  "Process the CHUNK according to MEDIA-TYPE using PROCESS.")

(cl-defmethod plz-media-type-else ((_ (eql nil)) error)
  "Transform the ERROR into a format suitable for MEDIA-TYPE."
  error)

(defun plz-media-type-parse (header)
  "Parse the Content-Type HEADER.

Return a cons cell where the car is the MIME type, and the cdr is
an alist of parameters."
  (unless (or (null header) (string-blank-p header))
    (let* ((components (split-string header ";"))
           (mime-type (string-trim (car components)))
           (parameters-list (cdr components))
           (parameters-alist '()))
      (dolist (param parameters-list parameters-alist)
        (let* ((key-value (split-string param "="))
               (key (string-trim (car key-value)))
               (value (string-trim (cadr key-value) "\"")))
          (setq parameters-alist (cons (cons key value) parameters-alist))))
      (let ((parts (split-string mime-type "/")))
        (plz-media-type
         :type (intern (car parts))
         :subtype (intern (cadr parts))
         :parameters (nreverse parameters-alist))))))

(defun plz-media-type--content-type (response)
  "Return the content type header of RESPONSE, or nil if it's not set."
  (let ((headers (plz-response-headers response)))
    (when-let (header (cdr (assoc 'content-type headers)))
      (plz-media-type-parse header))))

(defun plz-media--type-find (media-types media-type)
  "Lookup the MEDIA-TYPE in MEDIA-TYPES."
  (or (alist-get (plz-media-type-symbol media-type) media-types)
      (alist-get t media-types)
      (plz-media-type:application/octet-stream)))

(defun plz-media-type--of-response (media-types response)
  "Lookup the content type of RESPONSE in MEDIA-TYPES."
  (let ((media-type (plz-media-type--content-type response)))
    (clone (plz-media--type-find media-types media-type))))

(defvar-local plz-media-type--current nil
  "The media type of the process buffer.")

(defvar-local plz-media-type--position nil
  "The position in the process buffer.")

(defvar-local plz-media-type--response nil
  "The response of the process buffer.")

(defun plz-media-type-process-filter (process media-types chunk)
  "The process filter that handles different content types.

PROCESS is the process.

MEDIA-TYPES is an association list from media type to an
instance of a content type class.

CHUNK is a part of the HTTP body."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (if-let (media-type plz-media-type--current)
            (let ((coding-system (plz-media-type-coding-system media-type))
                  (response plz-media-type--response))
              (setf (plz-response-body response)
                    (decode-coding-string chunk coding-system))
              (plz-media-type-process media-type process response))
          (progn
            (save-excursion
              (goto-char (process-mark process))
              (insert chunk)
              (set-marker (process-mark process) (point)))
            (goto-char (point-min))
            (when (re-search-forward plz-http-end-of-headers-regexp nil t)
              (let ((body-start (point)))
                (goto-char (point-min))
                (let* ((response (prog1 (plz--response) (widen)))
                       (media-type (plz-media-type--of-response media-types response))
                       (coding-system (plz-media-type-coding-system media-type)))
                  (setq-local plz-media-type--current media-type)
                  (when-let (body (plz-response-body response))
                    (when (> (length body) 0)
                      (setf (plz-response-body response)
                            (decode-coding-string body coding-system))
                      (delete-region body-start (point-max))
                      (set-marker (process-mark process) (point))
                      (plz-media-type-process media-type process response)))
                  (setf (plz-response-body response) nil)
                  (setq-local plz-media-type--response response))))))
        (when moving
          (goto-char (process-mark process)))))))

;; Content Type: application/octet-stream

(defclass plz-media-type:application/octet-stream (plz-media-type)
  ((type :initform 'application)
   (subtype :initform 'octet-stream)))

(cl-defmethod plz-media-type-else ((media-type plz-media-type:application/octet-stream) error)
  "Transform the ERROR into a format suitable for MEDIA-TYPE."
  (let ((response (plz-error-response error)))
    (setf (plz-error-response error) (plz-media-type-then media-type response))
    error))

(cl-defmethod plz-media-type-then ((media-type plz-media-type:application/octet-stream) response)
  "Transform the RESPONSE into a format suitable for MEDIA-TYPE."
  (ignore media-type)
  (setf (plz-response-body response) (buffer-string))
  response)

(cl-defmethod plz-media-type-process ((media-type plz-media-type:application/octet-stream) process chunk)
  "Process the CHUNK according to MEDIA-TYPE using PROCESS."
  (ignore media-type)
  (save-excursion
    (goto-char (process-mark process))
    (insert (plz-response-body chunk))
    (set-marker (process-mark process) (point))))

;; Content Type: application/json

(defclass plz-media-type:application/json (plz-media-type:application/octet-stream)
  ((subtype :initform 'json)
   (array-type
    :documentation "Specifies which Lisp type is used to represent arrays.  It can be
`array' (the default) or `list'."
    :initarg :array-type
    :initform 'array
    :type symbol)
   (false-object
    :documentation "Specifies which object to use to represent a JSON false value. It
defaults to `:json-false'."
    :initarg :false-object
    :initform :json-false)
   (null-object
    :documentation "Specifies which object to use to represent a JSON null value.  It
defaults to `nil`."
    :initarg :null-object
    :initform nil)
   (object-type
    :documentation "Specifies which Lisp type is used to represent objects.  It can
be `hash-table', `alist' (the default) or `plist'."
    :initarg :object-type
    :initform 'alist
    :type symbol)))

(defun plz-media-type--parse-json-object (media-type)
  "Parse the JSON object in the current buffer according to MEDIA-TYPE."
  (with-slots (array-type false-object null-object object-type) media-type
    (json-parse-buffer :array-type array-type
                       :false-object false-object
                       :null-object null-object
                       :object-type object-type)) )

(cl-defmethod plz-media-type-then ((media-type plz-media-type:application/json) response)
  "Transform the RESPONSE into a format suitable for MEDIA-TYPE."
  (setf (plz-response-body response) (plz-media-type--parse-json-object media-type))
  response)

;; Content Type: application/json (array of objects)

(defclass plz-media-type:application/json-array (plz-media-type:application/json)
  ((handler
    :documentation "A function that will be called for each object in the JSON array."
    :initarg :handler
    :type (or function symbol))))

(defun plz-media-type:application/json-array--parse-next (media-type)
  "Parse a single line of the newline delimited JSON MEDIA-TYPE."
  (cond ((looking-at "\\[")
         (delete-char 1))
        ((looking-at "[ ,\n\r]")
         (delete-char 1))
        ((looking-at "\\]")
         (delete-char 1))
        ((not (eobp))
         (ignore-errors
           (let ((begin (point)))
             (prog1 (plz-media-type--parse-json-object media-type)
               (delete-region begin (point))))))))

(defun plz-media-type:application/json-array--parse-stream (media-type)
  "Parse all lines of the newline delimited JSON MEDIA-TYPE in the PROCESS buffer."
  (with-slots (handler) media-type
    (unless plz-media-type--position
      (setq-local plz-media-type--position (point)))
    (goto-char plz-media-type--position)
    (let ((object (plz-media-type:application/json-array--parse-next media-type)))
      (setq-local plz-media-type--position (point))
      (while object
        (setq-local plz-media-type--position (point))
        (when (functionp handler)
          (funcall handler object))
        (setq object (plz-media-type:application/json-array--parse-next media-type))))))

(cl-defmethod plz-media-type-process ((media-type plz-media-type:application/json-array) process chunk)
  "Process the CHUNK according to MEDIA-TYPE using PROCESS."
  (ignore media-type)
  (cl-call-next-method media-type process chunk)
  (plz-media-type:application/json-array--parse-stream media-type))

(cl-defmethod plz-media-type-then ((media-type plz-media-type:application/json-array) response)
  "Transform the RESPONSE into a format suitable for MEDIA-TYPE."
  (ignore media-type)
  (plz-media-type:application/json-array--parse-stream media-type)
  response)

;; Content Type: application/x-ndjson

(defclass plz-media-type:application/x-ndjson (plz-media-type:application/json)
  ((subtype :initform 'x-ndjson)
   (handler
    :documentation "A function that will be called for each line that contains a JSON object."
    :initarg :handler
    :initform nil
    :type (or function null symbol))))

(defconst plz-media-type:application/x-ndjson--line-regexp
  (rx (* not-newline) (or "\r\n" "\n" "\r"))
  "Regular expression matching a JSON Object line.")

(defun plz-media-type:application/x-ndjson--parse-line (media-type)
  "Parse a single line of the newline delimited JSON MEDIA-TYPE."
  (when (looking-at plz-media-type:application/x-ndjson--line-regexp)
    (prog1 (plz-media-type--parse-json-object media-type)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun plz-media-type:application/x-ndjson--parse-stream (media-type)
  "Parse all lines of the newline delimited JSON MEDIA-TYPE in the PROCESS buffer."
  (with-slots (handler) media-type
    (unless plz-media-type--position
      (setq-local plz-media-type--position (point)))
    (goto-char plz-media-type--position)
    (when-let (object (plz-media-type:application/x-ndjson--parse-line media-type))
      (while object
        (setq-local plz-media-type--position (point))
        (when (functionp handler)
          (funcall handler object))
        (setq object (plz-media-type:application/x-ndjson--parse-line media-type))))))

(cl-defmethod plz-media-type-process ((media-type plz-media-type:application/x-ndjson) process chunk)
  "Process the CHUNK according to MEDIA-TYPE using PROCESS."
  (cl-call-next-method media-type process chunk)
  (plz-media-type:application/x-ndjson--parse-stream media-type))

(cl-defmethod plz-media-type-then ((media-type plz-media-type:application/x-ndjson) response)
  "Transform the RESPONSE into a format suitable for MEDIA-TYPE."
  (plz-media-type:application/x-ndjson--parse-stream media-type)
  response)

;; Content Type: application/xml

(defclass plz-media-type:application/xml (plz-media-type:application/octet-stream)
  ((subtype :initform 'xml)))

(cl-defmethod plz-media-type-then ((media-type plz-media-type:application/xml) response)
  "Transform the RESPONSE into a format suitable for MEDIA-TYPE."
  (with-slots (array-type false-object null-object object-type) media-type
    (setf (plz-response-body response) (libxml-parse-html-region))
    response))

;; Content Type: text/html

(defclass plz-media-type:text/html (plz-media-type:application/xml)
  ((type :initform 'text)
   (subtype :initform 'xml)))

(defvar plz-media-types
  `((application/json . ,(plz-media-type:application/json))
    (application/octet-stream . ,(plz-media-type:application/octet-stream))
    (application/xml . ,(plz-media-type:application/xml))
    (text/html . ,(plz-media-type:text/html))
    (t . ,(plz-media-type:application/octet-stream)))
  "Alist from media type to content type.")

(defun plz-media-type--handle-sync-http-error (error media-types)
  "Handle the synchronous HTTP ERROR using MEDIA-TYPES."
  (let* ((msg (cadr error))
         (plzerror (caddr error)))
    (signal (car error)
            (cond
             ((plz-error-response plzerror)
              (let ((response (plz-error-response plzerror)))
                (if-let (media-type (plz-media-type--of-response media-types response))
                    (list msg (with-temp-buffer
                                (when-let (body (plz-response-body response))
                                  (insert body)
                                  (goto-char (point-min)))
                                (plz-media-type-else media-type plzerror)))
                  (cdr error))))))))

(defun plz-media-type--handle-sync-error (error media-types)
  "Handle the synchronous ERROR using MEDIA-TYPES."
  (if (eq 'plz-http-error (car error))
      (plz-media-type--handle-sync-http-error error media-types)
    (signal (car error) (cdr error))))

(cl-defun plz-media-type-request
    (method
     url
     &rest rest &key headers body else finally noquery
     (as 'string)
     (body-type 'text)
     (connect-timeout plz-connect-timeout)
     (decode t decode-s)
     (then 'sync)
     (timeout plz-timeout))
  "Request METHOD from URL with curl.
Return the curl process object or, for a synchronous request, the
selected result.

HEADERS may be an alist of extra headers to send with the
request.

BODY may be a string, a buffer, or a list like `(file FILENAME)'
to upload a file from disk.

BODY-TYPE may be `text' to send BODY as text, or `binary' to send
it as binary.

AS selects the kind of result to pass to the callback function
THEN, or the kind of result to return for synchronous requests.
It may be:

- `buffer' to pass the response buffer, which will be narrowed to
  the response body and decoded according to DECODE.

- `binary' to pass the response body as an un-decoded string.

- `string' to pass the response body as a decoded string.

- `response' to pass a `plz-response' structure.

- `file' to pass a temporary filename to which the response body
  has been saved without decoding.

- `(file FILENAME)' to pass FILENAME after having saved the
  response body to it without decoding.  FILENAME must be a
  non-existent file; if it exists, it will not be overwritten,
  and an error will be signaled.

- `(stream :through PROCESS-FILTER)' to asynchronously stream the
  HTTP response.  PROCESS-FILTER is an Emacs process filter
  function, and must accept two arguments: the curl process
  sending the request and a chunk of the HTTP body, which was
  just received.

- A function, which is called in the response buffer with it
  narrowed to the response body (suitable for, e.g. `json-read').

If DECODE is non-nil, the response body is decoded automatically.
For binary content, it should be nil.  When AS is `binary',
DECODE is automatically set to nil.

THEN is a callback function, whose sole argument is selected
above with AS; if the request fails and no ELSE function is
given (see below), the argument will be a `plz-error' structure
describing the error.  Or THEN may be `sync' to make a
synchronous request, in which case the result is returned
directly from this function.

ELSE is an optional callback function called when the request
fails (i.e. if curl fails, or if the HTTP response has a non-2xx
status code).  It is called with one argument, a `plz-error'
structure.  If ELSE is nil, a `plz-curl-error' or
`plz-http-error' is signaled when the request fails, with a
`plz-error' structure as the error data.  For synchronous
requests, this argument is ignored.

NOTE: In v0.8 of `plz', only one error will be signaled:
`plz-error'.  The existing errors, `plz-curl-error' and
`plz-http-error', inherit from `plz-error' to allow applications
to update their code while using v0.7 (i.e. any `condition-case'
forms should now handle only `plz-error', not the other two).

FINALLY is an optional function called without argument after
THEN or ELSE, as appropriate.  For synchronous requests, this
argument is ignored.

CONNECT-TIMEOUT and TIMEOUT are a number of seconds that limit
how long it takes to connect to a host and to receive a response
from a host, respectively.

NOQUERY is passed to `make-process', which see.

When the HTTP response is streamed, the buffering in the curl
output stream is turned off and the PROCESS-FILTER may be called
multiple times, depending on the size of the HTTP body.  It is
the user's responsibility to understand and process each chunk,
and to construct the finalized response if necessary.  There are
no guarantees regarding the chunk, such as being line-based or
not.
\(To silence checkdoc, we mention the internal argument REST.)"
  ;; FIXME(v0.8): Remove the note about error changes from the docstring.
  ;; FIXME(v0.8): Update error signals in docstring.
  (declare (indent defun))
  (ignore as) ;; TODO: Handle as?
  (if-let (media-types (pcase as
                         (`(media-types ,media-types)
                          media-types)))
      (condition-case error
          (let* ((plz-curl-default-args (cons "--no-buffer" plz-curl-default-args))
                 (result (plz method url
                           :as 'buffer
                           :body body
                           :body-type body-type
                           :connect-timeout connect-timeout
                           :decode decode
                           :else (when (functionp else)
                                   (lambda (error)
                                     (funcall else (plz-media-type-else
                                                    plz-media-type--current
                                                    error))))
                           :finally (when (functionp finally)
                                      (lambda () (funcall finally)))
                           :headers headers
                           :noquery noquery
                           :process-filter (lambda (process chunk)
                                             (plz-media-type-process-filter process media-types chunk))
                           :timeout timeout
                           :then (cond
                                  ((symbolp then) then)
                                  ((functionp then)
                                   (lambda (_)
                                     (funcall then (plz-media-type-then
                                                    plz-media-type--current
                                                    plz-media-type--response))))))))
            (cond ((bufferp result)
                   (with-current-buffer result
                     (plz-media-type-then plz-media-type--current plz-media-type--response)))
                  ((processp result)
                   result)
                  (t (user-error "Unexpected response: %s" result))))
        (plz-error (plz-media-type--handle-sync-error error media-types)))
    (apply #'plz (append (list method url) rest))))

;;;; Footer

(provide 'plz-media-type)

;;; plz-media-type.el ends here
