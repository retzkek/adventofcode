(in-package aoc)

(defparameter *examples*
  '("mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    "bvwbjplbgvbhsrlpgdmjqwftvncz"
    "nppdvjthqldpwncqszvftbrmjlhg"
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(defun find-marker (len buf)
  (april-c (with (:state :in ((len len))))
           "{(len-1)+(+/ ¨ ≠ ¨ ↓⍉len ((≢⍵)+1) ⍴ ⍵) ⍳ len}"
           buf))

(defun part1 (buf)
  (find-marker 4 buf))

(mapcar 'part1 *examples*)

(part1 (first (input-as-lines 2022 6)))

(defun part2 (buf)
  (find-marker 14 buf))

(mapcar 'part2 *examples*)

(part2 (first (input-as-lines 2022 6)))
