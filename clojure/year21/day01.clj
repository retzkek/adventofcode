(ns year21.day01
  (:require [aoc]))

(defn count-increases [depths]
  (loop [prev -1
         depths depths
         cnt -1]
    (if (empty? depths)
      cnt
      (recur (first depths)
             (rest depths)
             (if (> (first depths) prev)
               (inc cnt)
               cnt)))))

(defn part1 []
  (count-increases (aoc/get-input-ints 2021 1)))

(defn count-sum-increases [depths]
  (let [windows (partition 3 1 depths)
        sums (map #(reduce + %) windows)]
    (count-increases sums)))

(defn part2 []
  (count-sum-increases (aoc/get-input-ints 2021 1)))

(comment
  (def ex [199
           200
           208
           210
           200
           207
           240
           269
           260
           263])
  (count-increases ex)
  (count-sum-increases ex)
  (part1)
  (part2)
  )
