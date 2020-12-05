(ns day1
  (:require
   [util :as util :refer [defrun]]))

(def numbers
  (map util/parse-int (util/read-input "day1")))

(defrun part1
  (let [deltas (->> numbers
                    (map #(- 2020 %))
                    (into #{}))
        matching (first (filter deltas numbers))]
    (* matching (- 2020 matching))))

(defn adding-up-to
  [xs n]
  (let [deltas (->> xs
                    (map #(- n %))
                    (into #{}))]
    (first (filter deltas xs))))

(defrun part2
  (->> numbers
       (map (fn [n]
              (let [target (- 2020 n)
                    match (adding-up-to numbers target)]
                [n match (when match (- target match))])))
       (filter (fn [[x y z]] (and x y z)))
       (first)
       (apply *)))
