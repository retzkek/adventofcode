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

(defn line->ints [line sep]
  (mapv #(Integer/parseInt %)
        (remove empty?
                (str/split line sep))))

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
  )
