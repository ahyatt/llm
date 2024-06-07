;;; plz-event-source.el --- Plz Event Source -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; URL: https://github.com/r0man/plz-event-source.el

;; This file is part of GNU Emacs.

;; It is temporarily vendored within the llm library.  Please DO NOT
;; depend on it!  It is subject to change.  Once we think this package
;; is stable, we will release it to GNU ELPA.  If no serious issues
;; are found, we plan to do this in Q4 of 2024.

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

;; This library provides a parser and an event source implementation
;; for the Server Sent Event (SSE) protocol.

;; See: https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'plz)
(require 'plz-media-type)
(require 'rx)

;; Event

(defclass plz-event-source-event ()
  ((data
    :accessor plz-event-source-event-data
    :initarg :data
    :initform nil
    :documentation "The event data.")
   (last-event-id
    :accessor plz-event-source-event-last-event-id
    :initarg :last-event-id
    :initform nil
    :documentation "The last event id."
    :type (or null string))
   (origin
    :accessor plz-event-source-event-origin
    :initarg :origin
    :initform nil
    :documentation "The event origin."
    :type (or null string))
   (type
    :accessor plz-event-source-event-type
    :initarg :type
    :initform 'message
    :documentation "The event type."
    :type symbol))
  "The server sent event class.")

;; Parser

(defclass plz-event-source-parser ()
  ((buffer
    :documentation "The name of the buffer to read events from."
    :initarg :buffer
    :type string)
   (events
    :initarg :events
    :initform nil
    :documentation "The queue of events to dispatch."
    :type (list-of plz-event-source-event))
   (data-buffer
    :initarg :data-buffer
    :initform ""
    :documentation "Data buffer."
    :type string)
   (event-type-buffer
    :initarg :event-type-buffer
    :initform ""
    :documentation "Event type buffer."
    :type string)
   (last-event-id
    :initarg :last-event-id
    :initform ""
    :documentation "Last event id."
    :type string)
   (last-event-id-buffer
    :initarg :last-event-id-buffer
    :initform ""
    :documentation "Last event id buffer."
    :type string)
   (position
    :initarg :position
    :initform 0
    :type integer
    :documentation "The position in the buffer."
    :type integer))
  "The server sent event stream parser.")

(defconst plz-event-source-parser--end-of-line-regexp
  (rx (or "\r\n" "\n" "\r"))
  "Regular expression matching the end of a line.")

(defconst plz-event-source-parser--line-regexp
  (rx (* not-newline) (or "\r\n" "\n" "\r"))
  "Regular expression matching a line of the event source stream.")

(defun plz-event-source-parser--parse-bom (line)
  "Parse the Byte Order Mark (BOM) from LINE."
  (if (string-prefix-p "\uFEFF" line)
      (substring line 1)
    line))

(defun plz-event-source-parser--looking-at-line-p ()
  "Return non-nil if the current line matches the event source line regexp."
  (looking-at plz-event-source-parser--line-regexp))

(defun plz-event-source-parser--parse-line ()
  "Return non-nil if the current line matches the event source line regexp."
  (when (looking-at plz-event-source-parser--line-regexp)
    (string-trim-right (delete-and-extract-region (match-beginning 0) (match-end 0))
                       plz-event-source-parser--end-of-line-regexp)))

(defun plz-event-source-parser--dispatch-event (parser)
  "Dispatch an event from PARSER to registered listeners."
  (with-slots (data-buffer event-type-buffer events last-event-id last-event-id-buffer) parser
    (setf last-event-id last-event-id-buffer)
    (if (string-empty-p data-buffer)
        (setf data-buffer ""
              event-type-buffer "")
      (progn
        (setf data-buffer (string-trim-right data-buffer "\n"))
        (let ((event (plz-event-source-event
                      :data data-buffer
                      :last-event-id (unless (string-blank-p last-event-id)
                                       last-event-id)
                      :origin (buffer-name)
                      :type (if (string-blank-p event-type-buffer)
                                'message
                              (intern event-type-buffer)))))
          (setf data-buffer ""
                event-type-buffer "")
          (setf events (cons event events))
          event)))))

(defun plz-event-source-parser--process-event (parser field value)
  "Process the FIELD and VALUE from PARSER as a event."
  (ignore field)
  (with-slots (event-type-buffer) parser
    (setf event-type-buffer value)))

(defun plz-event-source-parser--process-data (parser field value)
  "Process the FIELD and VALUE from PARSER as data."
  (ignore field)
  (with-slots (data-buffer) parser
    (setf data-buffer (concat data-buffer value "\n"))))

(defun plz-event-source-parser--process-id (parser field value)
  "Process the FIELD and VALUE from PARSER as event id."
  (ignore field)
  (unless (string-match "\u0000" value)
    (with-slots (last-event-id-buffer) parser
      (setf last-event-id-buffer value))))

(defun plz-event-source--process-field (parser field value)
  "Process the FIELD and VALUE from PARSER."
  (cond ((equal "event" field)
         (plz-event-source-parser--process-event parser field value))
        ((equal "data" field)
         (plz-event-source-parser--process-data parser field value))
        ((equal "id" field)
         (plz-event-source-parser--process-id parser field value))))

(defun plz-event-source-parser--process-line (parser line)
  "Parse a LINE of the event stream PARSER and dispatch events."
  (cond ((string-prefix-p ":" line))
        ((string-blank-p line)
         (plz-event-source-parser--dispatch-event parser))
        ((string-match ":" line)
         (let ((field (substring line 0 (match-beginning 0)))
               (value (substring line (match-end 0))))
           (plz-event-source--process-field parser field
                                            (if (string-prefix-p " " value)
                                                (substring value 1)
                                              value))))
        (t (plz-event-source--process-field parser line ""))))

(defun plz-event-source-parser--insert (parser string)
  "Insert STRING into the buffer of the event PARSER."
  (with-slots (buffer events position) parser
    (with-current-buffer (get-buffer buffer)
      (insert string)
      (while (plz-event-source-parser-parse-line parser))
      events)))

(defun plz-event-source-parser--end-of-headers ()
  "Return the end of headers position in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward plz-http-end-of-headers-regexp nil t)
    (point)))

(defun plz-event-source-parser-parse-line (parser)
  "Parse a line from the event stream in the PARSER buffer."
  (with-slots (buffer position) parser
    (with-current-buffer buffer
      (save-excursion
        (goto-char position)
        (when-let (line (plz-event-source-parser--parse-line))
          (setf position (point))
          (plz-event-source-parser--process-line parser line)
          line)))))

(defun plz-event-source-parser-parse (parser)
  "Parse the event stream in the the PARSER buffer."
  (with-slots (buffer handlers) parser
    (with-current-buffer (get-buffer buffer)
      (goto-char (point-min))
      (while (not (eobp))
        (when-let (line (plz-event-source-parser--parse-line))
          (plz-event-source-parser--process-line parser line))))))

;; Event Source

(defclass plz-event-source ()
  ((errors
    :initarg :errors
    :documentation "The errors of the event source.")
   (handlers
    :initarg :handlers
    :initform nil
    :documentation "Registered event handlers.")
   (last-event-id
    :initarg :last-event-id
    :initform ""
    :documentation "Last event id.")
   (options
    :initarg :options
    :documentation "The url of the event source."
    :type list)
   (ready-state
    :documentation "The ready state of the event source."
    :initarg :ready-state
    :initform 'closed
    :type (member closed connecting open))
   (url
    :initarg :url
    :documentation "The url of the event source."
    :type (or null string)))
  "The server sent event source class.")

(cl-defgeneric plz-event-source-open (source)
  "Open the event SOURCE.")

(cl-defgeneric plz-event-source-close (source)
  "Close the event SOURCE.")

(cl-defgeneric plz-event-source--insert (source data)
  "Insert DATA into the event SOURCE buffer, parse and dispatch events.")

(defun plz-event-source-add-listener (source type listener)
  "Add an event LISTENER for event TYPE to the event SOURCE."
  (with-slots (handlers) source
    (setf handlers (append handlers (list (cons type listener))))
    source))

(defun plz-event-source-remove-listener (source type listener)
  "Remove an event LISTENER for event TYPE from the event SOURCE."
  (with-slots (handlers) source
    (setf handlers (cl-remove-if (lambda (pair)
                                   (and (eq (car pair) type)
                                        (eq (cdr pair) listener)))
                                 handlers))
    source))

(defun plz-event-source--dispatch-event (source event)
  "Dispatch the EVENT to the listeners of event SOURCE."
  (with-slots (handlers) source
    (dolist (pair handlers)
      (when (equal (car pair) (oref event type))
        (let ((timer (timer-create)))
          (timer-set-time timer (current-time))
          (timer-set-function timer
                              (lambda (handler event)
                                (with-temp-buffer
                                  (funcall handler event)))
                              (list (cdr pair) event))
          (timer-activate timer))))))

(defun plz-event-source--dispatch-events (source events)
  "Dispatch the EVENTS to the listeners of event SOURCE."
  (dolist (event (reverse events))
    (plz-event-source--dispatch-event source event)))

;; Buffer event source

(defclass plz-event-source-buffer (plz-event-source)
  ((buffer
    :initarg :buffer
    :documentation "The event source buffer."
    :type string)
   (parser
    :initarg :parser
    :documentation "The event source parser."
    :type (or null plz-event-source-parser)))
  "A server sent event source using curl for HTTP.")

(cl-defmethod plz-event-source--insert ((source plz-event-source-buffer) data)
  "Insert DATA into the event SOURCE buffer, parse and dispatch events."
  (with-slots (parser) source
    (plz-event-source-parser--insert parser data)
    (with-slots (events) parser
      (plz-event-source--dispatch-events source events)
      (setf events nil))))

(defun plz-event-source--skip-proxy-headers ()
  "Skip proxy headers in current buffer."
  (when (looking-at plz-http-response-status-line-regexp)
    (let* ((status-code (string-to-number (match-string 2)))
           (reason-phrase (match-string 3)))
      (when (and (equal 200 status-code)
                 (equal "Connection established" reason-phrase))
        (re-search-forward "\r\n\r\n" nil t)))))

(defun plz-event-source--skip-redirect-headers ()
  "Skip HTTP redirect headers in current buffer."
  (when (and (looking-at plz-http-response-status-line-regexp)
             (member (string-to-number (match-string 2)) '(301 302 303 307 308)))
    (re-search-forward "\r\n\r\n" nil t)))

(defun plz-event-source--buffer-start-position ()
  "Return the start position of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (plz-event-source--skip-proxy-headers)
    (while (plz-event-source--skip-redirect-headers))
    (re-search-forward plz-http-end-of-headers-regexp nil t)
    (point)))

(cl-defmethod plz-event-source-open ((source plz-event-source-buffer))
  "Open a connection to the URL of the event SOURCE."
  (with-slots (buffer errors options ready-state parser) source
    (with-current-buffer (get-buffer-create buffer)
      (let ((event (plz-event-source-event :type 'open)))
        (setf ready-state 'connecting)
        (setf parser (plz-event-source-parser
                      :buffer buffer
                      :position (plz-event-source--buffer-start-position)))
        (setf ready-state 'open)
        (plz-event-source--dispatch-event source event)
        source))))

(cl-defmethod plz-event-source-close ((source plz-event-source-buffer))
  "Close the connection of the event SOURCE."
  (with-slots (buffer ready-state) source
    (let ((event (plz-event-source-event :type 'close)))
      (setf ready-state 'closed)
      (plz-event-source--dispatch-event source event)
      source)))

(defclass plz-event-source-http (plz-event-source)
  ((process
    :initarg :process
    :documentation "The process of the event source."
    :type (or null process))
   (response
    :initarg :response
    :documentation "The plz HTTP response."
    :type (or null plz-response)))
  "A server sent event source using curl for HTTP.")

(defun plz-event-source--media-types (source)
  "Return the media types of the event SOURCE."
  (with-slots (handlers) source
    (let ((media-type (plz-event-source:text/event-stream :events handlers)))
      (cons (cons 'text/event-stream media-type) plz-media-types))))

(cl-defmethod plz-event-source-open ((source plz-event-source-http))
  "Open a connection to the URL of the event SOURCE."
  (with-slots (errors options process ready-state response url) source
    (setf ready-state 'connecting)
    (setf response nil)
    (setf process (plz-media-type-request
                    (or (alist-get 'method options) 'get) url
                    :as `(media-types ,(plz-event-source--media-types source))
                    :body (alist-get 'body options)
                    :headers (alist-get 'headers options)
                    :then (lambda (object)
                            (setf response object))
                    :else (lambda (object)
                            (setf errors (push object errors))
                            (setf response (plz-error-response object)))
                    :finally (lambda ()
                               (setf ready-state 'closed))))
    source))

(cl-defmethod plz-event-source-close ((source plz-event-source-http))
  "Close the connection of the event SOURCE."
  (with-slots (process ready-state) source
    (delete-process process)
    (setf ready-state 'closed)))

;; Content Type: text/event-stream

(defclass plz-event-source:text/event-stream (plz-media-type:application/octet-stream)
  ((coding-system :initform 'utf-8)
   (type :initform 'text)
   (subtype :initform 'event-stream)
   (events :documentation "Association list from event type to handler."
           :initarg :events
           :initform nil
           :type list))
  "Media type class that handles the processing of HTTP responses
in the server sent events format.  The HTTP response is processed
in a streaming way.  The :events slot of the class can be set to
an association list from event type symbol to a handler function.
Whenever a new event is parsed and emitted the handler for the
corresponding event type will be called with two arguments, an
instance of the underlying event source class and an event.  The
body slot of the plz-response structure passed to the THEN and
ELSE callbacks will always be set to nil.")

(defvar-local plz-event-source--current nil
  "The event source of the current buffer.")

(cl-defmethod plz-media-type-else ((_ plz-event-source:text/event-stream) error)
  "Transform the ERROR into a format suitable for MEDIA-TYPE."
  (let* ((source plz-event-source--current)
         (event (plz-event-source-event :type 'error :data error)))
    (plz-event-source-close source)
    (plz-event-source--dispatch-event source event)
    error))

(cl-defmethod plz-media-type-process ((media-type plz-event-source:text/event-stream) process chunk)
  "Process the CHUNK according to MEDIA-TYPE using PROCESS."
  (unless plz-event-source--current
    (let* ((response (make-plz-response
                      :status (plz-response-status chunk)
                      :headers (plz-response-headers chunk)))
           (source (plz-event-source-open
                    (plz-event-source-buffer
                     :buffer (buffer-name (process-buffer process))
                     :handlers (seq-map
                                (lambda (pair)
                                  (let ((type (car pair))
                                        (handler (cdr pair)))
                                    (cond
                                     ((equal 'open type)
                                      (cons type (lambda (event)
                                                   (setf (oref event data) response)
                                                   (funcall handler event))))
                                     ((equal 'close type)
                                      (cons type (lambda (event)
                                                   (setf (oref event data) response)
                                                   (funcall handler event))))
                                     (t pair))))
                                (oref media-type events))))))
      (setq-local plz-event-source--current source)))
  (let ((body (plz-media-type-decode-coding-string media-type (plz-response-body chunk))))
    (plz-event-source--insert plz-event-source--current body)
    (set-marker (process-mark process) (point))))

(cl-defmethod plz-media-type-then ((media-type plz-event-source:text/event-stream) response)
  "Transform the RESPONSE into a format suitable for MEDIA-TYPE."
  (plz-event-source-close plz-event-source--current)
  (cl-call-next-method media-type response)
  (setf (plz-response-body response) nil)
  response)

(provide 'plz-event-source)
;;; plz-event-source.el ends here
