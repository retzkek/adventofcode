(ns year23.day02
  (:require [aoc]
            [instaparse.core :as insta]
            [clojure.walk :refer [postwalk]]))

(def parse 
  (insta/parser 
    "S = game+
    game = id draws <'\\n'*>
    id = <'Game '> #'\\d+' <': '>
    draws = (draw <'; '?>)+
    draw = (count <', '?>)+ &('; '|'\\n')
    count = #'\\d+' <' '> ('blue'|'red'|'green')"))

(defn games [parsed]
  (postwalk (fn [s]
              (cond
                (vector? s)
                (let [[tag & rest] s] 
                  (cond 
                    (keyword? tag) 
                    (case tag
                      :S (vec rest)
                      :game (reduce #(merge %1 (apply hash-map %2)) {} rest)
                      :id (vector tag (Integer/parseInt (first rest)))
                      :draws (vector tag (vec rest))
                      :draw (apply merge rest)
                      :count {(keyword (last rest)) (Integer/parseInt (first rest))}
                      s)
                    :else s))
                :else s)) parsed))

(defn game-possible? 
  "Returns the game :id if it can be played with the number of cubes of each 
  type given, or false otherwise."
  [cubes g]
  (reduce 
    (fn [acc draw] 
      (and acc 
           (reduce-kv #(and %1 (<= %3 (cubes %2))) true draw)
           (:id g)))
    true (:draws g)))

(defn sum-possible-games [inp]
  (->> (parse (str inp "\n"))
       ; [:S
       ;  [:game
       ;   [:id "1"]
       ;   [:draws
       ;    [:draw [:count "3" "blue"] [:count "4" "red"]]
       ;    [:draw [:count "1" "red"] [:count "2" "green"] [:count "6" "blue"]]
       ;    [:draw [:count "2" "green"]]]]]
       games
       ; [{:id 1,
       ;   :draws [{:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2}]}]
       (map #(game-possible? {:red 12, :green 13, :blue 14} %))
       ; (1 2 false false 5)
       (filter identity)
       ; (1 2 5)
       (reduce + 0)))

(comment
  (def testdata "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
  (def testdata "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  (parse testdata :unhide :all)
  (sum-possible-games testdata) ; 8

  (sum-possible-games (aoc/get-input 2023 2)) ; 2795
  )

(defn minimum-cubes
  "Returns the minimum number of cubes of each type required to play the game."
  [g]
  (apply merge-with #(max %1 %2) (:draws g)))

(defn sum-power [inp]
  (->> (parse (str inp "\n"))
       games
       (map minimum-cubes)
       ; ({:blue 6, :red 4, :green 2}
       ;  {:blue 4, :green 3, :red 1}
       ;  {:green 13, :blue 6, :red 20}
       ;  {:green 3, :red 14, :blue 15}
       ;  {:red 6, :blue 2, :green 3})
       (map #(reduce * (vals %)))
       ; (48 12 1560 630 36)
       (reduce +)))

(comment
  (merge-with #(max %1 %2) {:a 1} {:b 2} {:a 3})
  (sum-power testdata) ; 2286

  (sum-power (aoc/get-input 2023 2)) ; 75561
  )

