(ns year24.day03
  (:require [aoc]
            [instaparse.core :as insta]))

(defn find-muls [inp]
  (mapv #(vector (Integer/parseInt (fnext %)) (Integer/parseInt (last %)))
        (re-seq #"mul\((\d+),(\d+)\)" inp)))

(defn sum-muls [muls]
  (->> muls
       (mapv #(reduce * %))
       (reduce +)))

(defn part1 [inp]
  (->> inp
       find-muls
       sum-muls))

(comment
  (def test1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  (part1 test1) ; 161
  (part1 (aoc/get-input 2024 03)) ; 191183308
  )

(def parse
  (insta/parser 
    "S = (mul|do|dont|<noise>)+
    noise = #'.'
    mul = 'mul(' #'\\d+' ',' #'\\d+' ')'
    do = 'do()'
    dont = 'do\\'nt()'"))

(defn find-instructions [inp]
  (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" inp))

(comment
  (def test2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (parse test2)
  (find-instructions test2)
  (part2 test2) ; 161
  (part1 (aoc/get-input 2024 03)) ; 191183308
  )
