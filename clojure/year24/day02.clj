(ns year24.day02
  (:require [aoc]))

(defn safe [rep]
  (let [pairs (partition 2 1 rep)
        changes (mapv #(apply - %) pairs)
        increasing? (every? #(> % 0) changes)
        decreasing? (every? #(< % 0) changes)
        safe? (every? #(< 0 (abs %) 4) changes)]
    (and (or increasing? decreasing?) safe?)))

(defn part1 [inp]
  (->> inp
       (mapv #(aoc/line->ints % #" "))
       (mapv safe)
       (reduce #(+ %1 (if %2 1 0)) 0)))

(comment
  (def inp "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
  (part1 (aoc/lines inp)) ; 2
  (part1 (aoc/get-input-lines 2024 02)) ; 371
  )

(defn alts [v]
  (mapv (fn [i]
          (condp = i
            0 (subvec v 1)
            (dec (count v)) (subvec v 0 i)
            (concat 
              (subvec v 0 i)
              (subvec v (inc i)))))
        (range (count v))))

(defn mostly-safe [rep]
  (or 
    (safe rep)
    (reduce #(if (safe %2) (reduced true) false) 
            false
            (alts rep))))

(defn part2 [inp]
  (->> inp
       (mapv #(aoc/line->ints % #" "))
       (mapv mostly-safe)
       (reduce #(+ %1 (if %2 1 0)) 0)))

(comment
  (alts [1 2 3 4 5])
  (part2 (aoc/lines inp)) ; 4
  (part2 (aoc/get-input-lines 2024 02)) ; 426
  )
