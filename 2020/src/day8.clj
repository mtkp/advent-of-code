(ns day8
  (:require
   [clojure.string :as string]
   [util :as util :refer [defrun]]))

(defn parse-instruction
  [s]
  (let [[c v] (string/split s #" ")]
    {:command (keyword c)
     :value (util/parse-int v)}))

(defmulti execute (fn [ins _ _] (:command ins)))
(defmethod execute :nop [_ p acc] [(inc p) acc])
(defmethod execute :acc [ins p acc] [(inc p) (+ acc (:value ins))])
(defmethod execute :jmp [ins p acc] [(+ p (:value ins)) acc])

(def input
  (mapv parse-instruction (util/read-input "day8")))

(defn compute-acc
  [program]
  (loop [p 0
         acc 0
         visited #{}]
    (if (or (visited p) (= p (count program)))
      acc
      (let [ins (get program p)
            [p' acc'] (execute ins p acc)]
        (recur p' acc' (conj visited p))))))

(defrun part1
  (compute-acc input))

(defn contains-loop?
  [program]
  (loop [p 0
         acc 0
         visited #{}]
    (cond
      (visited p) true
      (= p (count program)) false
      :else (let [ins (get program p)
                  [p' acc'] (execute ins p acc)]
              (recur p' acc' (conj visited p))))))

(defn generate-mutations
  [program]
  (reduce
   (fn [acc p]
     (let [ins (get program p)
           mutate (fn [command]
                    (assoc-in program [p :command] command))]
       (cond
         (= :jmp (:command ins)) (conj acc (mutate :nop))
         (= :nop (:command ins)) (conj acc (mutate :jmp))
         :else acc)))
   (list)
   (range (count program))))

(defrun part2
  (->> (generate-mutations input)
       (remove contains-loop?)
       (first)
       (compute-acc)))
