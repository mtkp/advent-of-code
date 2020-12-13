(ns day12
  (:require
   [util :as util :refer [defrun]]))

(defn parse-instruction
  [s]
  (let [[_ action value] (re-matches #"(\w)(\d+)" s)]
    {:action (keyword action)
     :value (util/parse-int value)}))

(def input
  (map parse-instruction (util/read-input "day12")))

(defn heading-direction
  [heading]
  (if-let [direction (get {0 :N
                           90 :E
                           180 :S
                           270 :W} heading)]
    direction
    (throw (ex-info "No direction for heading" {:heading heading}))))

(defn rotate-right
  [heading degrees]
  (mod (+ heading degrees) 360))

(defn navigate
  [ship {:keys [action value] :as instruction}]
  (case action
    :N (update ship :north + value)
    :S (update ship :north - value)
    :E (update ship :east + value)
    :W (update ship :east - value)
    :R (update ship :heading rotate-right value)
    :L (update ship :heading rotate-right (* -1 value))
    :F (navigate ship {:action (heading-direction (:heading ship))
                       :value value})))

(defrun part1
  (let [end (reduce navigate {:east 0 :north 0 :heading 90} input)]
    (+ (Math/abs (:north end)) (Math/abs (:east end)))))

(defn rotate-waypoint
  [waypoint direction degrees]
  (let [degrees-right (cond-> degrees
                        (= :L direction) (* -1)
                        true             (mod 360))]
    (loop [degrees-right degrees-right
           waypoint waypoint]
      (if (zero? degrees-right)
        waypoint
        (recur (- degrees-right 90) {:north (* -1 (:east waypoint))
                                     :east (:north waypoint)})))))

(defn waypoint-navigate
  [{:keys [ship waypoint]}
   {:keys [action value] :as instruction}]
  (case action
    (:N :S :E :W) {:ship ship
                   :waypoint (navigate waypoint instruction)} ;; reuse part1
    (:R :L) {:ship ship
             :waypoint (rotate-waypoint waypoint action value)}
    :F {:ship (reduce (partial merge-with +) ship (repeat value waypoint))
        :waypoint waypoint}))

(defrun part2
  (let [{:keys [ship]} (reduce waypoint-navigate {:ship {:north 0
                                                         :east 0}
                                                  :waypoint {:north 1
                                                             :east 10}} input)]
    (+ (Math/abs (:north ship)) (Math/abs (:east ship)))))
