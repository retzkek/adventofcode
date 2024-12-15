(ns year24.day13
  (:require [aoc]
            [instaparse.core :as insta]))

(def test1 "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(def parse
  (insta/parser
    "S = mach (<blank> mach)+
    blank = #'\\n\\n'
    mach = A B prize
    A = <'Button A: X+'> d <', Y+'> d <'\\n'>
    B = <'Button B: X+'> d <', Y+'> d <'\\n'>
    d = #'\\d+'
    prize = <'Prize: X='> d <', Y='> d <'\\n'>?"))

(defn parse-inp [inp]
  (->> (parse inp)
       (insta/transform
         {:d #(Integer/parseInt %)
          :mach (fn [a b p] {:a (subvec a 1) :b (subvec b 1) :prize (subvec p 1)})
          :S (fn [& rest] rest)})))

(defn combs [{:keys [a b prize]}]
  (remove nil? 
          (for [na (range) :while (< (* na (first a)) (first prize))
                nb (range) :while (<= (+ (* na (second a)) (* nb (second b))) (second prize))]
            (when (and (= (+ (* na (first a)) (* nb (first b))) (first prize))
                       (= (+ (* na (second a)) (* nb (second b))) (second prize)))
              [na nb]))))

(defn cost [[a b]]
  (+ (* a 3) b))

(defn cheapest-comb [m]
  (let [cs (combs m)]
    (if (empty? cs)
      0
      (apply min (map cost cs)))))

(defn part1 [inp]
  (->> (parse-inp inp)
       #_(map combs)
       (map cheapest-comb)
       (reduce +)))

(comment
  (part1 test1) ; 480
  (part1 (aoc/get-input 2024 13)) ; 37128
  )
