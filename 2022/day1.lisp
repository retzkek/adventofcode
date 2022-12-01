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
  (aoc:with-input-stream (in 2022 1)
    (max-calories in)))

(time (princ (part1)))

(defun top3 (ns)
  (let ((sorted (sort ns '>)))
    (list (car sorted) (cadr sorted) (caddr sorted))))

(defun part2 ()
  (aoc:with-input-stream (in 2022 1)
    (reduce '+ (top3 (calories-per-elf  in)))))

(time (princ (part2)))
