(ns util
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [clojure.stacktrace :as stacktrace]))

;; parsing

(defn parse-int
  [o]
  (cond
    (string? o) (Integer/parseInt o)
    :else (throw (ex-info (format "unable to parse [%s] (%s)" o (type o)) {:object o}))))

(defn read-input
  ([file]
   (read-input file {:split-lines true}))
  ([file opts]
   (cond-> (slurp (str "resources/" file))
     (:split-lines opts) (string/split-lines))))

(defn partition-lines
  ([lines]
   (partition-lines lines {:delimiter ""}))
  ([lines {:keys [delimiter]}]
   (->> lines
        (partition-by (partial = delimiter))
        (remove (partial = (list delimiter))))))

;; logic

(defn xor
  [b1 b2] ;; TODO generalize
  (and (or b1 b2) (not (and b1 b2))))

;; math

(defn exp [x n]
  (reduce * (repeat n x)))

;; part execution

(defrecord RunError [^Exception e]
  Object
  (toString [_]
    (with-out-str
      (stacktrace/print-stack-trace e))))

(defn run-error?
  [o]
  (instance? RunError o))

(defmacro defrun
  [n & body]
  `(defn ~n
     ([] (~n {}))
     ([opts#]
      (let [report# (get opts# :report? true)
            ns# ~(do *ns*)
            _# (when report#
                 (print (format "Running %s -- %s\n> " (.getName ns#) ~(str n))))
            res# (try
                   (do ~@body)
                   (catch Exception e#
                     (->RunError e#)))]
        (if report#
          (println (format "%s" res#))
          (if (run-error? res#)
            (throw (:e res#))
            res#))))))

;; debugging 

(defn spy [x]
  (println "spying:")
  (pprint x)
  x)

;; data structues

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn min-element
  [coll]
  (apply min coll))

(defn max-element
  [coll]
  (apply max coll))
