(in-package aoc)

(defun parse (stream)
  (loop with ops = nil
        for line = (read-line stream nil)
        until (null line)
        do (cond
             ((string= line "noop") (push 0 ops))
             ((string= (subseq line 0 4) "addx")
              (push 0 ops)
              (push (parse-integer (subseq line 5)) ops)))
        finally (return (coerce (reverse ops) 'vector))))

(with-input-from-string (in *example1*) (parse in))

(defun part1 (stream)
  (april-f (with (:state :in ((in (parse stream))))) "
x←(1,1++⍀in)
+/{⍵×⍵⊃x}¨20+40×(⍳ 6)-1
"))

(with-input-from-string (in *example1*) (part1 in))

(with-input-stream (in 2022 10) (part1 in))

(defparameter *example1* "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")
