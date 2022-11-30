(defpackage aoc
  (:use cl)
  (:export fetch-input
           open-input-stream
           input-lines
           input-ints))
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
   'drakma:cookie-jar
   :cookies (list (make-instance 'drakma:cookie
                                  :name "session"
                                  :domain ".adventofcode.com"
                                  :value (load-token)))))

(defun fetch-input (year day path)
  "Fetch input from AOC site and save to file given by PATH."
  (let ((url (format nil "https://adventofcode.com/~d/day/~d/input" year day)))
    (note "fetching input from ~s" url)
    (multiple-value-bind (data response-code)
        (drakma:http-request url
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

(defun input-lines (year day)
  "Get AOC input for YEAR and DAY as list of strings."
  (let ((in (open-input-stream year day)))
    (loop for line = (read-line in nil)
          until (null line)
          collect line)))

(defun input-ints (year day &key (as 'list))
  "Get AOC input for YEAR and DAY as sequence (default 'list) of ints."
  (map as 'parse-integer (input-lines year day)))
