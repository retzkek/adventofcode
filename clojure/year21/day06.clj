(ns year21.day06
  (:require [aoc]))

(defn simulate-day [fish]
  (reduce (fn [fish a-fish]
            (if (= a-fish 0)
              (conj fish 6 8)
              (conj fish (dec a-fish))))
          []
          fish))

(defn simulate-days [fish n-days]
  (if (> n-days 0)
    (recur (simulate-day fish) (dec n-days))
    fish))


(defn part1 []
  (-> (aoc/get-input-lines 2021 6)
      first
      (aoc/line->ints #",")
      (simulate-days 80)
      count))

;; going to be too many fish to track indiviusally for part 2. So how about we
;; instead track the number of fish with n days left?
(defn count-fish [fish]
  (reduce (fn [fish a-fish]
            (update fish a-fish #(inc (or % 0))))
          {}
          fish))

(defn update-fish [counted-fish]
  (reduce (fn [new-fish n]
            (assoc new-fish n (counted-fish (inc n) 0)))
          {8 (counted-fish 0 0)
           7 (counted-fish 8 0)
           6 (+ (counted-fish 0 0) (counted-fish 7 0))}
          (range 0 6)))

(defn simulate-days2 [counted-fish n-days]
  (if (> n-days 0)
    (recur (update-fish counted-fish) (dec n-days))
    (reduce + (vals counted-fish))))

(defn part2 []
  (-> (aoc/get-input-lines 2021 6)
      first
      (aoc/line->ints #",")
      count-fish
      (simulate-days2 256)))

(comment
  (def ex [3 4 3 1 2])

  (simulate-day (simulate-day ex))

  (count (simulate-days ex 18))

  (update-fish (update-fish (update-fish (count-fish ex))))

  (simulate-days2 (count-fish ex) 18)

  )
