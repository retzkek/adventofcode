(in-package aoc)
(defparameter *example1* "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(with-input-from-string (ex *example1*) (read-lines ex))

(defun calories-per-elf (stream)
  (loop with total = 0
        for line = (read-line stream nil)
        until (null line)
        when (= (length line) 0)
          collect total
          and do (setq total 0)
        else
          do (setq total (+ total (parse-integer line)))))

(with-input-from-string (ex *example1*)
  (calories-per-elf ex))

(defun max-calories (stream)
  (apply 'max (calories-per-elf stream)))

(with-input-from-string (ex *example1*)
  (max-calories ex))

(defun part1 ()
  (with-input-stream (in 2022 1)
    (max-calories in)))

(time (princ (part1)))

(defun top3 (ns)
  (let ((sorted (sort ns '>)))
    (list (car sorted) (cadr sorted) (caddr sorted))))

(defun part2 ()
  (with-input-stream (in 2022 1)
    (reduce '+ (top3 (calories-per-elf  in)))))

(time (princ (part2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; april
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ex1 (coerce
             (mapcar (lambda (x) (or x 0))
                     (with-input-from-string (ex *example1*)
                       (read-lines-as-ints ex)))
             'vector))
(april-c "{(⍵≠0)}" ex1) ;; find zeroes
(april-c "{(⍵≠0) ⊆ ⍵}" ex1) ;; parition by elf
(april-c "{⌈/ +/ ¨ (⍵≠0) ⊆ ⍵}" ex1)

(defun part1-april ()
  (april-c "{⌈/ +/ ¨ (⍵≠0) ⊆ ⍵}" (input-as-ints 2022 1 :as 'vector :blanks-as 0)))
