(ns day11
  (:require
   [util :as util :refer [defrun]]))

(def input
  (mapv vec (util/read-input "day11")))

(def directions
  [[-1 -1] [-1 0] [-1 1] [1 -1] [1 0] [1 1] [0 -1] [0 1]])

(defn simulate-model-step
  [state behavior-fn]
  (vec (map-indexed
        (fn [row xs]
          (vec (map-indexed (fn [col _] (behavior-fn state row col)) xs)))
        state)))

(defn simulate-model
  [initial-state behavior-fn]
  (loop [state initial-state
         previous nil]
    (if (= state previous)
      state
      (recur (simulate-model-step state behavior-fn) state))))

(defn get-adjacent-seats
  [state row col]
  (->> directions
       (map (fn [[row+ col+]] (get-in state [(+ row row+) (+ col col+)])))
       (remove nil?)
       (frequencies)
       (merge {\L 0 \. 0 \# 0})))

(defn adjacent-neighbors-behavior
  [state row col]
  (let [x (get-in state [row col])
        adjacent (get-adjacent-seats state row col)]
    (cond
      (and (= x \L) (<= (get adjacent \#) 0)) \#
      (and (= x \#) (<= 4 (get adjacent \#))) \L
      :else x)))

(defrun part1
  (->> (simulate-model input adjacent-neighbors-behavior)
       (reduce into)
       (filter #{\#})
       (count)))

(defn find-seat
  [state row col row+ col+]
  (loop [row' (+ row row+)
         col' (+ col col+)]
    (let [x (get-in state [row' col'])]
      (if (= \. x)
        (recur (+ row' row+) (+ col' col+)) ;; keep looking
        x))))

(defn get-visible-seats
  [state row col]
  (->> directions
       (map (fn [[row+ col+]] (find-seat state row col row+ col+)))
       (remove nil?)
       (frequencies)
       (merge {\L 0 \# 0})))

(defn visible-neighbors-behavior
  [state row col]
  (let [x (get-in state [row col])
        visible (get-visible-seats state row col)]
    (cond
      (and (= x \L) (<= (get visible \#) 0)) \#
      (and (= x \#) (<= 5 (get visible \#))) \L
      :else x)))

(defrun part2
  (->> (simulate-model input visible-neighbors-behavior)
       (reduce into)
       (filter #{\#})
       (count)))
