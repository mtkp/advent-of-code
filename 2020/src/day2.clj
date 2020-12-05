(ns day2
  (:require
   [util :as util :refer [defrun]]))

(def regex #"(\d+)-(\d+) ([a-z]): ([a-z]+)")

(defn parse-line
  [s]
  (let [[_ lo hi target password] (re-matches regex s)]
    {:min (Integer/parseInt lo)
     :max (Integer/parseInt hi)
     :target (first target)
     :password password}))

(def input
  (map parse-line (util/read-input "day2")))

(defn valid-password-1?
  [entry]
  (let [times (count (filter #{(:target entry)} (:password entry)))]
    (<= (:min entry) times (:max entry))))

(defrun part1 (count (filter valid-password-1? input)))

(defn valid-password-2?
  [{:keys [target] :as entry}]
  (let [password-chars (vec (:password entry))
        char1 (get password-chars (dec (:min entry)))
        char2 (get password-chars (dec (:max entry)))]
    (util/xor
     (= char1 target)
     (= char2 target))))

(defrun part2
  (count (filter valid-password-2? input)))

(comment
  (count [1  1])

  (filter #(= \s %) "swoops")

  (first "s")

  (get (vec "helloworld") 99)

  (valid-password? (parse-line "1-3 a: abcde")))
