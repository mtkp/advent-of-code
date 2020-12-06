(ns day6
  (:require
   [util :as util :refer [defrun]]))

(def input
  (util/partition-lines
   (util/read-input "day6")))

(defn any-group-yes
  [group-lines]
  (reduce (partial into) #{} group-lines))

(defrun part1
  (->> input
       (map any-group-yes)
       (map count)
       (reduce +)))

(defn every-group-yes
  [group-lines]
  (reduce #(filter (set %2) %1) (first group-lines) group-lines))

(defrun part2
  (->> input
       (map every-group-yes)
       (map count)
       (reduce +)))
