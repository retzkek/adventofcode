;(ql:quickload :drakma)
;(ql:quickload :april)
;(ql:quickload :cl-ppcre)
(defpackage aoc
  (:use cl
        drakma
        april
        cl-ppcre)
  (:export fetch-input
           with-input-stream
           read-lines
           input-as-lines
           read-lines-as-ints
           input-as-ints))
(in-package aoc)

(defun note (&rest args)
  (apply 'format (cons *error-output*
                       (cons (format nil "aoc: ~a" (car args)) (cdr args)))))

(defparameter *aocd-conf-path* (merge-pathnames ".config/aocd/" (user-homedir-pathname)))
(defun load-token ()
  "Load AOC session token from env var or file."
  (or (uiop:getenv "AOC_SESSION_TOKEN")
      (with-open-file (in (merge-pathnames "token" *aocd-conf-path*))
        (read-line in nil))))

(defparameter *cookie-jar* nil)
(defun make-cookie-jar ()
  (make-instance
   'cookie-jar
    :cookies (list (make-instance 'cookie
                                  :name "session"
                                  :domain ".adventofcode.com"
                                  :value (load-token)))))

(defun fetch-input (year day path)
  "Fetch input from AOC site and save to file given by PATH."
  (let ((url (format nil "https://adventofcode.com/~d/day/~d/input" year day)))
    (note "fetching input from ~s" url)
    (multiple-value-bind (data response-code)
        (http-request url
                             :cookie-jar (or *cookie-jar*
                                             (setq *cookie-jar* (make-cookie-jar))))
      (if (>= response-code 400)
          (error "Response ~d while fetching input from ~s." response-code url)
          (with-open-file (out path :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create)
            (write-string data out))))))

(defun open-input-stream (year day)
  "Open AOC input for YEAR and DAY as stream"
  (let ((path (merge-pathnames (format nil "~d/~d/input.txt" year day) *aocd-conf-path*)))
    (when (not (probe-file path))
      (ensure-directories-exist path)
      (fetch-input year day path))
    (open path :direction :input)))

(defmacro with-input-stream ((var year day) &body body)
  "Fetch input for AOC problem for YEAR and DAY into input stream bound
  to VAR, then evaluate BODY and close the stream"
  `(let ((,var (open-input-stream ,year ,day)))
     (unwind-protect
          (progn ,@body)
       (close ,var))))

(defun read-lines (stream)
  "Read lines from STREAM until EOF"
  (loop for line = (read-line stream nil)
        until (null line)
        collect line))

(defun input-as-lines (year day)
  "Get AOC input for YEAR and DAY as list of strings."
  (with-input-stream (in year day)
    (read-lines in)))

(defun read-lines-as-ints (stream &key (as 'list) (blanks-as nil))
  "Read lines from STREAM until EOF, returning sequence (default 'list) of ints"
  (map as (lambda (x) (or (parse-integer x :junk-allowed t) blanks-as)) (read-lines stream)))

(defun input-as-ints (year day &key (as 'list) (blanks-as nil))
  "Get AOC input for YEAR and DAY as sequence (default 'list) of ints."
  (with-input-stream (in year day)
    (read-lines-as-ints in :as as :blanks-as blanks-as)))
