(in-package aoc)

(defparameter *examples*
  '("mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    "bvwbjplbgvbhsrlpgdmjqwftvncz"
    "nppdvjthqldpwncqszvftbrmjlhg"
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(defun find-marker (buf)
  (april-c "{3+(+/ ¨≠ ¨ ↓⍉4 ((≢⍵)+1) ⍴ ⍵) ⍳ 4}" buf))

(mapcar 'find-marker *examples*)

(find-marker (first (input-as-lines 2022 6)))
