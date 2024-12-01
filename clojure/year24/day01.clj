(ns year24.day01
  (:require [aoc]
            [clojure.string :as str]))

(defn lines->vecs [ls]
  (reduce (fn [[left right] x]
            (let [[l r] (mapv #(Integer/parseInt %) (str/split x #" +"))] 
              [(conj left l) (conj right r)])) 
          [[] []]
          ls))

(defn distance [ls]
  (->> (lines->vecs ls)
       (mapv sort)
       (apply mapv -)
       (mapv abs)
       (reduce +)))

(comment
  (def ex "3   4
4   3
2   5
1   3
3   9
3   3") 
  (distance (str/split-lines ex)) ; 11
  (distance (aoc/get-input-lines 2024 1)) ; 765748
  )

(defn freq [xs]
  (reduce (fn [acc x]
            (update acc x #(inc (or % 0))))
          {}
          xs))

(defn similarity [ls]
  (let [[l r] (lines->vecs ls)
        rc (freq r)]
    (->> (mapv #(* % (get rc % 0)) l)
         (reduce +))))

(comment
  (similarity (str/split-lines ex)) ; 31
  (similarity (aoc/get-input-lines 2024 1)) ; 27732508
  )
