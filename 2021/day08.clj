(ns retzkek.aoc21.day08
  (:require [retzkek.aoc :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[patterns outputs] (str/split line #" \| ")
        patterns (str/split patterns #" ")
        outputs (str/split outputs #" ")]
    [patterns outputs]))

(defn count-unique [digits]
  (count (filter #(let [len (count %)]
                    (or
                     (= len 2)
                     (= len 4)
                     (= len 3)
                     (= len 7)))
                 digits)))

(defn part1 []
  (->> (aoc/get-input-lines 2021 8)
       (map #(-> %
                 parse-line
                 second
                 count-unique))
       (reduce +)))

;; two segments => 1
;; three segnemtn => 7
;; four segment => 4
;; seven segment => 8
;; five segments => 2, 3, 5
;;   superset of 1 => 3
;;   subset of 3 U 4 => 5
;; six segments => 0, 6, 9
;;   not superset of 5 => 0
;;   3 U 4 => 9
(defn identify-digits [patterns]
  (let [grouped (group-by count (map set patterns))]
    (as-> {} known
      (assoc known 1 (first (grouped 2)))
      (assoc known 7 (first (grouped 3)))
      (assoc known 4 (first (grouped 4)))
      (assoc known 8 (first (grouped 7)))
      (assoc known 3 (first (filter
                             #(set/superset? % (known 1))
                             (grouped 5))))
      (assoc known 5 (first (filter
                             #(and (not= % (known 3))
                                   (set/subset? % (set/union (known 3) (known 4))))
                             (grouped 5))))
      (assoc known 2 (first (filter
                             #(and (not= % (known 3))
                                   (not= % (known 5)))
                             (grouped 5))))
      (assoc known 0 (first (filter
                             #(not (set/superset? % (known 5)))
                             (grouped 6))))
      (assoc known 9 (first (filter
                             #(= % (set/union (known 3) (known 4)))
                             (grouped 6))))
      (assoc known 6 (first (filter
                             #(and (not= % (known 0))
                                   (not= % (known 9)))
                             (grouped 6)))))))

(defn digits->number [digits]
  (reduce (fn [num digit] (+ (* 10 num) digit)) digits))

(defn decode-line [line]
  (let [[patterns digits] (parse-line line)
        mapping (set/map-invert (identify-digits patterns))]
    (->> digits
         (map #(mapping (set %)))
         digits->number)))

(defn part2 []
  (->> (aoc/get-input-lines 2021 8)
       (map decode-line)
       (reduce +)))

(comment
  (def ex (str/split "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce" #"\n"))

  (def pat (first (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")))

  (def dig (second (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")))

  (count-unique dig)
  (reduce + (map #(-> % parse-line second count-unique) ex))

  (group-by count pat)

  (identify-digits pat)
  (let [digits (set/map-invert (identify-digits pat))]
    (map #(digits (set %)) dig))

  (digits->number '(5 3 5 3))


  )
