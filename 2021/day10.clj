(ns retzkek.aoc21.day10
  (:require [retzkek.aoc :as aoc]
            [clojure.string :as str]))

(defn check-syntax [s]
  (loop [s s
         stack []]
    (if (empty? s)
      stack
      (let [c (first s)
            stack (cond
                    (some #(= % c) [\[ \( \{ \<])
                    (conj stack c)

                    (or
                     (and (= c \]) (= (peek stack) \[))
                     (and (= c \)) (= (peek stack) \())
                     (and (= c \}) (= (peek stack) \{))
                     (and (= c \>) (= (peek stack) \<)))
                    (pop stack))]
        (if (nil? stack)
          c
          (recur (rest s) stack))))))

(def score {\) 3
            \] 57
            \} 1197
            \> 25137})

(defn total-score [lines]
  (->> lines
       (map check-syntax)
       (filter char?)
       (map score)
       (reduce +)))

(defn part1 []
  (total-score (aoc/get-input-lines 2021 10)))

(def score2 {\( 1
             \[ 2
             \{ 3
             \< 4})

(defn autocomplete-score [chars]
  (reduce #(+ (* %1 5) %2) 0 (map score2 chars)))

(defn middle [xs]
  (nth (sort xs) (/ (count xs) 2)))

(defn total-autocomplete-score [lines]
  (->> lines
       (map check-syntax)
       (filter vector?)
       (map reverse)
       (map autocomplete-score)
       middle))

(defn part2 []
  (total-autocomplete-score (aoc/get-input-lines 2021 10)))

(comment
  (def ex (str/split-lines "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

  (check-syntax "{([(<{}[<>[]}>{[]{[(<()>")

  (check-syntax "[[<[([]))<([[{}[[()]]]")

  (check-syntax "[({(<(())[]>[[{[]{<()<>>")

  (total-score ex)

  (total-autocomplete-score ex)

    )
