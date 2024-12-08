(ns year24.day07
  (:require [aoc]
            [instaparse.core :as insta]
            [clojure.math.combinatorics :as combo]))

(def inp 
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(def parse
  (insta/parser 
    "S = eq+
    eq = value <':'> values <'\\n'?>
    values = (<' '> value)+
    value = #'\\d+'
    "))

(defn parse-inp [inp]
  (->> (parse inp)
       (insta/transform
         {:value #(Long/parseLong %)
          :values (fn [& rest] (vec rest))
          :eq (fn [r v] {:res r :vals v})
          :S (fn [& rest] rest)})))

(defn apply-fns [fns args]
  (loop [fns fns
         xs (rest args)
         acc (first args)]
    (if (empty? fns)
      acc
      (recur (rest fns)
             (rest xs)
             ((first fns) acc (first xs))))))

(defn cando? [{:keys [res vals]}]
  (let [pops (combo/selections [+ *] (dec (count vals)))] 
    (reduce (fn [_ x] 
              (when (= (apply-fns x vals) res)
                (reduced res))) 
            nil
            pops)))

(defn part1 [inp]
  (->> (parse-inp inp)
       (map cando?)
       (remove nil?)
       (reduce +)))


(comment
  (part1 inp) ; 3749
  (part1 (aoc/get-input 2024 07)) ; 3312271365652
  )

