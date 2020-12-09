(ns day9
  (:require
   [util :as util :refer [defrun]]))

(def input
  (map util/parse-int (util/read-input "day9")))

(defn adding-up-to
  [xs n]
  (let [deltas (->> xs
                    (map #(- n %))
                    (into #{}))]
    (first (filter deltas xs))))

(defn valid-xmas?
  [preamble x]
  (->> preamble
       (map (fn [n]
              (let [target (- 2020 n)
                    match (adding-up-to preamble x)]
                [n match (when match (- target match))])))
       (filter (fn [[x y z]] (and x y z)))
       (first)))

(valid-xmas? (take 25 input) (first (drop 25 input)))
(valid-xmas? [1 2 3] 4)
(valid-xmas? [1 2 3] 9)

(defrun part1
  (loop [preamble (util/queue (take 25 input))
         xmas (drop 25 input)]
    (let [n (first xmas)]
      (if (valid-xmas? preamble n)
        (recur (conj (pop preamble) n) (rest xmas))
        n))))

(defrun part2
  ;; 2415514 is too low
  ;; 2980044 -- i was adding up the first and last, not the min and max
  (let [target (part1 {:report? false})]
    (loop [available input
           length 0
           sum 0]
      (cond
        (and (pos? length)
             (= target sum)) (->> [util/min-element
                                   util/max-element]
                                  (map #(% (take length available)))
                                  (reduce +))
        (< target sum) (recur (drop 1 available) 0 0)
        :else (recur available (inc length) (+ sum (nth available length)))))))
