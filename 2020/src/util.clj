(ns util
  (:require
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

(defmacro defrun
  [n & body]
  `(defn ~n
     ([]
      (let [ns# ~(do *ns*)
            _# (print (format "Running %s -- %s\n> " (.getName ns#) ~(str n)))
            res# (try
                   (do ~@body)
                   (catch Exception e#
                     (->RunError e#)))]
        (println (format "%s" res#))))
     ([_#] (~n))))

;; debugging 

(defn spy [x]
  (println "spying:")
  (clojure.pprint/pprint x)
  x)
