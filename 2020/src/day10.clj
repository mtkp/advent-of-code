(ns day10
  (:require
   [util :as util :refer [defrun]]))

(defn jolt-sequence
  [adapters]
  (sort (into adapters [0 (+ 3 (util/max-element adapters))])))

(def input
  (map util/parse-int (util/read-input "day10")))

(defrun part1
  (let [differences (->> (jolt-sequence input)
                         (partition 2 1)
                         (map (fn [[a b]] (- b a)))
                         (frequencies))]
    (* (get differences 1) (get differences 3))))

(part1)

(def input2
  (map util/parse-int (util/read-input "day10short")))

(def input3
  (map util/parse-int (util/read-input "day10test")))

(defn contiguous-incrementing-sequences
  [xs]
  (loop [res [[(first xs)]]
         xs (rest xs)]
    (if (empty? xs)
      res
      (let [inc-last (inc (peek (peek res)))
            x (first xs)]
        (recur (if (= inc-last x)
                 (conj (pop res) (conj (peek res) x))
                 (conj res [x]))
               (rest xs))))))

(defn multiplier
  [n]
  (if (<= n 1)
    1
    (reduce + 1 (range (- n 1)))))

(defn possible-arrangements
  [adapters]
  (->> (jolt-sequence adapters)
       (contiguous-incrementing-sequences)
       (map count)
       (map multiplier)
       (reduce *)))

(defrun part2
  (possible-arrangements input))

(part2)
