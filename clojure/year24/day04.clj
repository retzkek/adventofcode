(ns year24.day04
  (:require [aoc]
            [clojure.string :as str]))

(defn transpose [strs] 
  (apply map 
         (fn [& rest] (apply str rest)) 
         strs))

(defn count-xmas [str]
  (count (re-seq #"XMAS" str)))

(defn part1 [inp]
  (->> inp
       (aoc/lines)
       ((fn [xs]
          (+ (reduce + (map count-xmas xs))
             (reduce + (map (comp count-xmas str/join reverse) xs))
             (reduce + (map #(+ (count-xmas %)
                                ((comp count-xmas str/join reverse) %)) 
                            (transpose xs)))
            )))))

(comment
  (aoc/get-input 2024 4)
  (def inp "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")
  (part1 inp)
  (count-xmas inp)
  (str/index-of "foobarbar" "bar")
  )
