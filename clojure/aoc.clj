(ns aoc
  (:require [aocd.core :as data]
            [clojure.string :as str])
  (:gen-class))

(defn get-input
  [year month]
  (data/input year month))

(defn get-input-lines
  [year month]
  (-> (get-input year month)
      (str/split #"\n")))

(defn get-input-ints
  [year month]
  (->> (get-input-lines year month)
       (mapv #(Integer/parseInt %))))

(defn lines [inp]
  (str/split inp #"\n"))

(defn line->ints [line sep]
  (mapv #(Integer/parseInt %)
        (remove empty?
                (str/split line sep))))

(defn get-input-map
  [year month]
  (mapv #(str/split % #"") (get-input-lines year month)))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))

(comment
  (get-input 2023 1)
  (get-input-lines 2023 1)
  (get-in (get-input-map 2023 16) [0 0])
  )
