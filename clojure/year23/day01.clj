(ns year23.day01
  (:require [aoc]
            [clojure.string :as str]))

(defn calibration [s]
  (->> (re-seq #"\d" s)
       (#(vector (first %) (last %)))
       (str/join "")
       (Integer/parseInt)))

(defn part1 []
  (->> (aoc/get-input-lines 2023 1)
       (mapv calibration)
       (reduce + 0)))

(comment
  (def inp (aoc/get-input-lines 2023 01))
  (calibration "1abc2")
  (mapv calibration 
        ["1abc2"
         "pqr3stu8vwx"
         "a1b2c3d4e5f"
         "treb7uchet"]) ; [12 38 15 77]
  (part1)
  )

(defn mapn [n]
  (or ({"one" 1
        "two" 2
        "three" 3
        "four" 4
        "five" 5
        "six" 6
        "seven" 7
        "eight" 8
        "nine" 9
        } n) n))

(defn calibration2 [s]
  (->> (vector 
         (re-find #"\d|one|two|three|four|five|six|seven|eight|nine" s)
         (str/reverse (re-find #"\d|enin|thgie|neves|xis|evif|ruof|eerht|owt|eno" 
                               (str/reverse s))))
       (map mapn)
       (str/join "")
       (Integer/parseInt)))

(defn part2 []
  (->> (aoc/get-input-lines 2023 1)
       (mapv calibration2)
       (reduce + 0)))

(comment
  (str/join "" (reverse "one|two|three|four|five|six|seven|eight|nine")) ; 
  (calibration2 "eightwo")
  (mapv calibration2
    ["two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
    "7sevenine"])
  (map mapn [1 2 "three"])
  (part2)
  )
