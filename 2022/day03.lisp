(in-package aoc)
(defparameter *example1* "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defun part1 (lines)
  (april-f (with (:state
                  :in ((in (coerce lines 'vector)))
                  :out (ans1))) "
shared←{
  l←⊃(⍴ ⍵) ÷ 2         ⍝ length of compartment
  p←(l l / 1 2) ⊆ ⍵    ⍝ split compartments
  ⊃ (⊃p)∩(2⊃p)         ⍝ find common items (why not ∩/p?)
}
prio←'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
ans1 ← +/ prio ⍳ shared ¨ in
"))

(part1 (with-input-from-string (in *example1*)
         (read-lines in)))

(part1 (input-as-lines 2022 3))
