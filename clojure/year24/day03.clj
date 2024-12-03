(ns year24.day03
  (:require [aoc]
            [clojure.string :as str]))

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

(defn find-instructions [inp]
  (mapv (fn [x]
          (condp #(str/starts-with? %2 %1) (first x)
            "mul" (vector :mul (Integer/parseInt (fnext x)) (Integer/parseInt (last x)))
            "don" [:off]
            "do" [:on])) 
        (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" inp)))

(defn run-instructions [prog]
  (:sum (reduce 
          (fn [state [inst & args]]
            ;(prn state)
            (case inst
              :on (assoc state :on true)
              :off (assoc state :on false)
              :mul (if (state :on)
                     (update state :sum #(+ (or %1 0) %2) (reduce * args))
                     state)))
          {:sum 0 :on true}
          prog)))

(defn part2 [inp]
  (->> inp
       find-instructions
       run-instructions))

(comment
  (def test2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (find-instructions test2) ; [[:mul 2 4] [:dont] [:mul 5 5] [:mul 11 8] [:do] [:mul 8 5]]
  (part2 test2) ; 48
  (part2 (aoc/get-input 2024 03)) ; 92082041
  )
