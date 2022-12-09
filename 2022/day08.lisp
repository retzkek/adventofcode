(in-pakage aoc)

(defparameter *example1* "30373
25512
65332
33549
35390")

(defun part1-debug (stream)
  (april-f (with (:state :in ((in (read-lines-as-ints stream :as 'vector))))) "
⎕←digits←⍉10∘⊥⍣¯1 ⊢ in
visible←{⍉↑≠¨↓⍉⌈⍀⍵}
⎕←'n'
⎕←n←visible digits
⎕←'w'
⎕←w←⍉visible ⍉digits
⎕←'e'
⎕←e←⌽⍉visible ⍉⌽digits
⎕←'s'
⎕←s←⊖visible ⊖digits
+⌿+⌿w∨n∨e∨s
"))

(defun part1 (stream)
  (april-f (with (:state :in ((in (read-lines-as-ints stream :as 'vector))))) "
digits←⍉10∘⊥⍣¯1 ⊢ in
visible←{⍉↑≠¨↓⍉⌈⍀⍵}
n←visible digits
w←⍉visible ⍉digits
e←⌽⍉visible ⍉⌽digits
s←⊖visible ⊖digits
+⌿+⌿w∨n∨e∨s
"))

(with-input-from-string (in *example1*) (part1 in))

(with-input-stream (in 2022 8) (part1 in))
