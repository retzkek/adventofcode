(in-package aoc)
(defparameter *example1* "    [D]   .
[N] [C]    .
[Z] [M] [P].
 1   2   3 .

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defun parse-map (stream)
  (loop with layer = nil
        for line = (read-line stream nil)
        until (= (length line) 0)
        collect (cl-ppcre:all-matches-as-strings "(    ?|\\[[A-Z]\\]| \\d )" line)))

(defstruct move n from to)

(defun parse-moves (stream)
  (loop for line = (read-line stream nil)
        until (null line)
        collect (cl-ppcre:register-groups-bind (n from to)
                    ("^move (\\d+) from (\\d+) to (\\d+)$" line)
                  (make-move :n (parse-integer n)
                             :from (parse-integer from)
                             :to (parse-integer to)))))

(defun parse (stream)
  (values
   (let* ((map (parse-map stream))
          (cleanmap
           (mapcar (lambda (x)
                     (mapcar (lambda (y)
                               (char y 1)) x))
                   (butlast map)))
          (len (length (car map)))
          (stacks (make-array len :initial-element nil)))
     (loop for layer in (reverse cleanmap) do
       (loop for i upto len
             and x in layer
             when (not (equal x #\ ))
             do
                (setf (aref stacks i) (cons x (aref stacks i)))))
     stacks)
   (parse-moves stream)))

(with-input-from-string (in *example1*)
  (parse in))

(defun part1 (stream)
  (multiple-value-bind (stacks moves) (parse stream)
    (loop for move in moves do
      (loop for i below (move-n move) do
        (push (pop (aref stacks (1- (move-from move))))
              (aref stacks (1- (move-to move))))))
    (coerce
     (loop for x across stacks
           collect (car x))
     'string)))

(with-input-from-string (in *example1*)
  (part1 in))

(with-input-stream (in 2022 5)
  (part1 in))

(defun part2 (stream)
  (multiple-value-bind (stacks moves) (parse stream)
    (loop for move in moves do
      (setf (aref stacks (1- (move-to move)))
            (nconc (subseq (aref stacks (1- (move-from move))) 0 (move-n move))
                   (aref stacks (1- (move-to move)))))
      (setf (aref stacks (1- (move-from move)))
            (nthcdr (move-n move) (aref stacks (1- (move-from move)))))
      (print stacks))
    (coerce
     (loop for x across stacks
           collect (car x))
     'string)))

(with-input-from-string (in *example1*)
  (part2 in))

(with-input-stream (in 2022 5)
  (part2 in))
