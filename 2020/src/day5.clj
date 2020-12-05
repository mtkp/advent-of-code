(ns day5
  (:require
   [util :as util :refer [defrun]]))

(def passes
  (util/read-input "day5"))

(defn binary-space-partition
  [xs lower?]
  ;; lower? is a function that takes an entry from xs and returns
  ;; truthy if we should go lower or falsey if we should go higher
  (let [space (dec (util/exp 2 (count xs)))]
    (loop [xs xs
           [lo hi] [0 space]]
      (cond
        (= lo hi) lo
        :else     (let [mid (int (- hi (/ (- hi lo) 2)))]
                    (recur
                     (rest xs) (if (lower? (first xs))
                                 [lo mid]
                                 [(inc mid) hi])))))))

(defn compute-seat-id
  [pass]
  (-> (binary-space-partition (take 7 pass) #{\F})
      (* 8)
      (+ (binary-space-partition (drop 7 pass) #{\L}))))

(defrun part1
  (->> passes
       (map compute-seat-id)
       (apply max)))

(defrun part2
  (->> passes
       (map compute-seat-id)
       (sort)
       (partition 2 1)
       (filter (fn [[x y]] (= 2 (- y x))))
       (first)
       (first)
       (inc)))
