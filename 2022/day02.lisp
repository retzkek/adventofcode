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
(defun apl (lines)
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

(apl (with-input-from-string (in *example1*)
       (read-lines in)))

(apl (input-as-lines 2022 2))
