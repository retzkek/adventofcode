(in-package aoc)
(defparameter *example1* "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defun parse-line (line)
  (map 'vector 'parse-integer (cl-ppcre:split "[,-]" line)))

(defun parse (stream)
  (mapcar 'parse-line (read-lines stream)))

(defun part1 (stream)
  (april-f (with (:state
                  :in ((in (coerce (parse stream) 'vector)))
                  :out (ans1))) "
expand←{(⍺-1) (1+⍵-⍺) / 0 1}
contains←{
m←⍺ ∧ ⍵          ⍝ find where arguments overlap
(⍺≡m) ∨ (⍵≡m)    ⍝ overlaps equal either argument?
}
f←{
contains/ ↓↑ expand/ 2 2 ⍴ ⍵
}
ans1← +/ f ¨ in
"))

(with-input-from-string (in *example1*)
    (part1 in))

(with-input-stream (in 2022 4)
  (part1 in))
