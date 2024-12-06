(ns year24.day05
  (:require [aoc]
            [instaparse.core :as insta]))

(def parse
  (insta/parser
    "S = rules <blank> updates
    rules = rule+
    rule = page <'|'> page <'\\n'>?
    blank = #'\\n\\n'
    page = #'\\d+'
    updates = update+
    update= page (<','> page)+ <'\\n'>?"))

(defn parse-inp [inp]
  (->> (parse inp)
       (insta/transform
         {:page #(Integer/parseInt %)
          :rule vector
          :update vector
          :rules (fn [& rest] {:rules (vec rest)})
          :updates (fn [& rest] {:updates (vec rest)})
          :S #(merge %1 %2)})))

(defn pairs [pages]
  (loop [ps (rest pages)
         f (first pages)
         acc nil]
    (if (empty? ps)
      acc
      (recur (rest ps)
             (first ps)
             (concat acc
                     (map #(vector f %) ps))))))

(defn update-ok? [rules pages]
  (let [anti-rules (map reverse rules)
        pp (pairs pages)]
    (not (reduce #(or %1 (boolean %2)) 
            false
            (map #(some #{%} pp) anti-rules)))))

(defn midel [seq]
  (nth seq (/ (count seq) 2)))

(defn part1 [inp]
  (let [{:keys [rules updates]} (parse-inp inp)]
    (->> (filter #(update-ok? rules %) updates)
         (map midel)
         (reduce +))))

(comment
  (def inp (slurp "year24/day05_test.txt"))
  (parse-inp inp)
  (concat [1 2 3] [4 5])
  (pairs [75 97 47 61 53])
  (update-ok? (:rules (parse-inp inp)) [75 97 47 61 53])
  (part1 inp) ; 143
  (part1 (aoc/get-input 2024 05)) ; 5391
  )
