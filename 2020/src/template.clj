(ns template)

(defn boilerplate
  [day]
  (format
   "(ns day%s
  (:require
   [util :as util :refer [defrun]]))

(def input
  (util/read-input \"day%s\"))

(defrun part1
  :part1)

(defrun part2
  :part2)"
   day day))

(defn generate
  [{:keys [day]}]
  (assert (and day (pos? day)) "Expected an integer")
  (spit (format "src/day%s.clj" day)
        (boilerplate day)
        :append true))
