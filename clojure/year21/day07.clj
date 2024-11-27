(ns year21.day07
  (:require [aoc]))

(defn fuel-cost [crabs pos]
  (reduce + (map #(Math/abs (- pos %)) crabs)))

(defn median [ns]
  (let [cnt (count ns)
        mid (dec (int (/ cnt 2)))
        sorted (sort ns)]
    (if (odd? cnt)
      (nth sorted mid)
      (/ (+ (nth sorted mid)
            (nth sorted (inc mid)))
         2))))

;; I _think_ the median is optimal? let's see...
(defn optimal-fuel-cost [crabs]
  (let [pos (median crabs)]
    (fuel-cost crabs pos)))

(defn part1 []
  (-> (aoc/get-input-lines 2021 7)
      first
      (aoc/line->ints #",")
      (optimal-fuel-cost)))

;; part 2
;; sigh just brute-force it...

(defn fuel-cost2 [crabs pos]
  (reduce + (map #(reduce + (range (inc (Math/abs (- pos %))))) crabs)))

(def fuel-cost2-memo (memoize fuel-cost2))

(defn optimal-fuel-cost2 [crabs]
  (let [min-crab (apply min crabs)
        max-crab (apply max crabs)
        costs (mapv #(fuel-cost2-memo crabs %)
                    (range (apply min crabs) (inc (apply max crabs))))]
    (apply min costs)))

(defn part2 []
  (-> (aoc/get-input-lines 2021 7)
      first
      (aoc/line->ints #",")
      (optimal-fuel-cost2)))



(comment
  (def ex (aoc/line->ints "16,1,2,0,4,2,7,1,2,14" #","))

  (int (Math/ceil (/ (reduce + ex) (count ex))))
  (sort ex)
  (count ex)

  (median ex)

  (for [i (range 0 17)]
    (fuel-cost ex i))

  (optimal-fuel-cost2 ex)

)
