(ns day3
  (:require
   [util :as util :refer [defrun]]))

(defprotocol Add
  (add [x y]))

(defrecord Point [x y]
  Add
  (add [a b]
    (->Point (+ (:x a) (:x b))
             (+ (:y a) (:y b)))))

(defn point [x y] (->Point x y))

(defn parse-tree-map-line
  [line]
  (fn [x object]
    ;; returns if the object at `x` matches `object`
    ;; 0123456
    ;; abcdefg
    ;; 1 => 0 => a
    ;; 8 => 0 => a
    (let [index (mod (dec x) (count line))]
      (= object (get line index)))))

(def input
  (let [lines (mapv parse-tree-map-line (util/read-input "day3"))]
    (fn [y]
      ;; returns the line at y
      (get lines (dec y)))))

(defn traverse
  [tree-map slope]
  (loop [pos (point 1 1)
         hits 0]
    (let [next-pos (add pos slope)
          line (tree-map (:y next-pos))]
      (if line
        (let [hit? (line (:x next-pos) \#)]
          (recur next-pos (if hit? (inc hits) hits)))
        hits))))

(defrun part1
  (traverse input {:x 3 :y 1}))

(defrun part2
  (let [slopes [{:x 1 :y 1}
                {:x 3 :y 1}
                {:x 5 :y 1}
                {:x 7 :y 1}
                {:x 1 :y 2}]]
    (->> slopes
         (map (partial traverse input))
         (reduce *))))
