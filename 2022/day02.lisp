(in-package aoc)
(defparameter *example1* "A Y
B X
C Z")

;; construct a score table as
;;   X Y Z
;; A 3 6 0
;; B 0 3 6
;; C 6 0 3
;;
(defun part1 (lines)
  (april-f (with (:state
                  :in ((in (coerce lines 'vector)))
                  :out (ans1))) "
scores ← 3 3 ⍴ 3 6 0 0 3 6 6 0 3
lefts ← 'ABC'
rights ← 'XYZ'
score ← {
l ← lefts ⍳ (⊃ ⍵)
r ← rights ⍳ (3 ⊃ ⍵)
r + l r ⌷ scores
}
ans1 ← +/ score ¨ in
"))

(part1 (with-input-from-string (in *example1*)
         (read-lines in)))

(part1 (input-as-lines 2022 2))

(defun part2 (lines)
  (april-f (with (:state
                  :in ((in (coerce lines 'vector)))
                  :out (ans2))) "
scores ← 3 3 ⍴ 3 6 0 0 3 6 6 0 3
lefts ← 'ABC'
rights ← 'X..Y..Z'
score ← {
l ← lefts ⍳ (⊃ ⍵)            ⍝ opponent's play
p ← (rights ⍳ (3 ⊃ ⍵)) - 1   ⍝ how many points we need
p + (l ⌷ scores) ⍳ p         ⍝ find the index (value) for our play, plus points we need
}
ans2 ← +/ score ¨ in
"))

(part2 (with-input-from-string (in *example1*)
         (read-lines in)))

(part2 (input-as-lines 2022 2))
