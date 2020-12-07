(ns day7
  (:require
   [clojure.string :as string]
   [util :as util :refer [defrun]]))

(defn parse-bag-color
  [s]
  (second (re-matches #"(.+) bags?" s)))

(defn parse-bag-quantity
  [s]
  (let [[_ n bag] (re-matches #"(\d+) (.+)" s)]
    {:count (util/parse-int n)
     :color (parse-bag-color bag)}))

(defn parse-rule
  [rule]
  (let [[_ outer inners] (re-matches #"(.+) contain (.+)\." rule)]
    {:color (parse-bag-color outer)
     :contains (->> (string/split inners #",")
                    (map string/trim)
                    (remove #{"no other bags"})
                    (map parse-bag-quantity))}))

(defn build-bag-graph
  [rules]
  (reduce
   (fn [acc rule]
     (reduce
      (fn [acc' color]
        (update acc' color (fnil conj #{}) (:color rule)))
      acc
      (map :color (:contains rule))))
   {}
   rules))

(defn traverse-bag-graph
  [start bag-graph]
  (loop [visited #{start}]
    (let [edges (->> visited
                     (mapcat (partial get bag-graph))
                     (remove visited))]
      (if (empty? edges)
        visited
        (recur (into visited edges))))))

(def input
  (util/read-input "day7"))

(defrun part1
  (->> input
       (map parse-rule)
       (build-bag-graph)
       (traverse-bag-graph "shiny gold")
       (remove #{"shiny gold"})
       (count)))

(defn build-bag-quantity-graph
  [rules]
  (reduce
   (fn [acc rule]
     (assoc acc (:color rule) (:contains rule)))
   {}
   rules))

(defn count-bag-quantities
  [start bag-graph]
  (->> (get bag-graph start)
       (map (fn [bag]
              (* (:count bag)
                 (inc (count-bag-quantities (:color bag) bag-graph)))))
       (reduce +)))

(defrun part2
  (->> input
       (map parse-rule)
       (build-bag-quantity-graph)
       (count-bag-quantities "shiny gold")))
