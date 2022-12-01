(defun fuel (mass)
  (april:april-c "{(⌊⍵÷3)-2}" mass))

(april:april "fuel←{(⌊⍵÷3)-2}
+/fuel 12 14 1969 100756")
(defun sum-fuel (masses)
  (april:april-c "{fuel←{(⌊⍵÷3)-2} ◊ +/fuel ⍵}" masses))

(mapcar 'fuel '(12 14 1969 100756))

(defun part1 ()
  (sum-fuel (aoc:input-as-ints 2019 1 :as 'vector)))
(time (princ (part1)))

(defun sum-total-fuel (mass)
  (let ((fuel (fuel mass)))
    (if (<= fuel 0)
        0
        (+ fuel (sum-total-fuel fuel)))))


(mapcar 'sum-total-fuel '(1969 100756))

(defun part2 ()
  (reduce '+ (mapcar 'sum-total-fuel (aoc:input-as-ints 2019 1))))

(time (princ (part2)))
