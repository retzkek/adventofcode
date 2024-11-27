(ns year21.day14
  (:require [aoc]
            [clojure.string :as str]))

(defn parse-input [lines]
  {:pairs (reduce (fn [pairs pair]
                    (update pairs (str/join pair) #(inc (or % 0))))
                  {}
                  (partition 2 1 (first lines)))
   :first (first (first lines))
   :last (last (first lines))
   :rules (reduce (fn [rules line]
                    (let [[_ pair element] (re-matches #"(\w\w) -> (\w)" line)]
                      (assoc rules pair
                             [(str/join [(first pair) element])
                              (str/join [element (last pair)])])))
                  {}
                  (drop 2 lines))})

(defn step [state]
  (assoc state :pairs
         (reduce-kv (fn [pairs pair n]
                      (if-let [[p1 p2] (get-in state [:rules pair])]
                        (-> pairs
                            (update p1 #(+ n (or % 0)))
                            (update p2 #(+ n (or % 0))))))
                    {}
                    (:pairs state))))

(defn step-n [state n]
  (if (zero? n)
    state
    (recur (step state) (dec n))))

(defn count-elements [state]
  (let [elements (reduce-kv (fn [els pair n]
                              (-> els
                                  (update (first pair) #(+ n (or % 0)))
                                  (update (second pair) #(+ n (or % 0)))))
                            {}
                            (:pairs state))]
    (reduce-kv (fn [els el n]
                 (assoc els el (/ n 2)))
               {}
               (-> elements
                   (update (:first state) inc)
                   (update (:last state) inc)))))

(defn part1 []
  (-> (aoc/get-input-lines 2021 14)
      (parse-input)
      (step-n 10)
      (count-elements)
      (#(- (apply max (vals %))
           (apply min (vals %))))))

(defn part2 []
  (-> (aoc/get-input-lines 2021 14)
      (parse-input)
      (step-n 40)
      (count-elements)
      (#(- (apply max (vals %))
           (apply min (vals %))))))

(comment
  (def ex (str/split-lines "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"))

  (parse-input ex)

  (:pairs (step (parse-input ex)))

  (-> (parse-input ex)
      (step-n 10)
      (count-elements))

  )
