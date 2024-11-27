(ns year21.day04
  (:require [aoc]
            [clojure.string :as str]))

(defn line->ints [line sep]
  (map #(Integer/parseInt %)
       (remove empty?
               (str/split line sep))))

(defn parse-input
  [lines]
  (reduce (fn [state line]
            (let [{:keys [boards]} state]
              (assoc state :boards
                     (cond
                       (empty? line) (conj boards [])
                       :else (assoc boards
                                    (dec (count boards))
                                    (into (last boards)
                                          (line->ints line #"\W+")))))))
          {:to-draw (line->ints (first lines) #",")
           :boards '[]}
          (rest lines)))

(def board-size 5)

(defn row [n board]
  (let [start (* n board-size)]
    (subvec board start (+ start board-size))))

(defn col [n board]
  (take-nth board-size (nthnext board n)))

(defn mark-all [n boards markers]
  (mapv (fn [board marks]
          (mapv (fn [bn m]
                  (if (= bn n) true m))
                board marks))
          boards markers))

(defn find-winner [boards markers]
  (if (empty? boards)
    nil
    (if (some true?
              (for [i (range board-size)]
                (or (every? true? (row i (first markers)))
                    (every? true? (col i (first markers))))))
      [(first boards) (first markers)]
      (recur (rest boards) (rest markers)))))

(defn score-winner [[board marks]]
  (reduce + (map #(if %1 0 %2) marks board)))

(defn play [state]
  (loop [state (assoc state
                      :markers (mapv #(vec (repeat (count %) false)) (:boards state))
                      :last-draw nil)]
    ;;(println state)
    (if (empty? (:to-draw state))
      "no winner!"
      (if-let [winner (find-winner (:boards state) (:markers state))]
        (* (score-winner winner) (:last-draw state))
        (let [[n & to-draw] (:to-draw state)]
          (recur (assoc state
                        :to-draw to-draw
                        :last-draw n
                        :markers (mark-all n (:boards state) (:markers state)))))))))

(defn part1 []
  (->> (aoc/get-input-lines 2021 4)
       parse-input
       play))

(defn find-winners [markers]
  (map #(some true?
              (for [i (range board-size)]
                (or (every? true? (row i %))
                    (every? true? (col i %)))))
       markers))

(defn play-all [state]
  (reduce (fn [state n]
            (let [markers (mark-all n (:boards state) (:markers state))]
              (assoc state
                     :markers  markers
                     :winners (conj (:winners state) (find-winners markers)))))
          (assoc state
                 :markers (mapv #(vec (repeat (count %) false)) (:boards state))
                 :winners [])
          (:to-draw state)))

(defn find-last-winner [state]
  "return a game state containing only the last board to win, and the numbers
  needed to draw for it to win"
  (loop [winners (reverse (:winners state))
         numbers (reverse (:to-draw state))
         last-drawn nil]
    (if (some nil? (first winners))
      (let [i (.indexOf (first winners) nil)]
        {:boards [((:boards state) i)]
         :to-draw (reverse (conj numbers last-drawn))})
      (recur (rest winners) (rest numbers) (first numbers)))))

(defn score-boards [state]
  (map #(* (score-winner [%1 %2]) (last (:to-draw state)))
       (:boards state)
       (:markers state)))

(defn part2 []
  (->> (aoc/get-input-lines 2021 4)
       parse-input
       play-all
       find-last-winner
       play-all
       score-boards
       first))


(comment

  (def ex (str/split "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7" #"\n"))

  (parse-input ex)

  (def my-board [22 13 17 11 0 8 2 23 4 24 21 9 14 16 7 6 10 3 18 5 1 12 20 15 19])

  (find-winner [my-board] [(vec (repeat 25 true))])

  (play (parse-input ex))

  (play-all (parse-input ex))

  (find-last-winner (play-all (parse-input ex)))

  )
