(ns day4
  (:require
   [clojure.string :as string]
   [util :as util :refer [defrun]]))

(def required-fields
  ["byr" ; (Birth Year)
   "iyr" ; (Issue Year)
   "eyr" ; (Expiration Year)
   "hgt" ; (Height)
   "hcl" ; (Hair Color)
   "ecl" ; (Eye Color)
   "pid" ; (Passport ID)
   ;"cid" ; (Country ID)
   ])

(defn parse-passport
  [s]
  (->> (string/split s #"\s+")
       (map #(string/split % #"\:"))))

(string/split "hello:world" #":")

(def input
  (->> (util/read-input "day4")
       (partition-by #{""})
       (map (fn [parts] (string/join " " parts)))
       (remove string/blank?)
       (map parse-passport)))

(defn valid-passport-structure?
  [passport]
  (let [fields (into #{} (map (fn [[k _]] k) passport))]
    (every? fields required-fields)))

(defrun part1
  (count (filter valid-passport-structure? input)))

(def passport-spec
  {"byr" (fn [s]
           (<= 1920 (util/parse-int s) 2002)) ; (Birth Year)
   "iyr" (fn [s]
           (<= 2010 (util/parse-int s) 2020)) ; (Issue Year)
   "eyr" (fn [s]
           (<= 2020 (util/parse-int s) 2030)) ; (Expiration Year)
   "hgt" (fn [s]
           (let [[_ n unit] (re-matches #"(\d+)(cm|in)" s)]
             (case unit
               "cm" (<= 150 (util/parse-int n) 193)
               "in" (<= 59 (util/parse-int n) 76)
               false))) ; (Height)
   "hcl" (fn [s]
           (re-matches #"\#[0-9a-f]{6,6}" s)) ; (Hair Color)
   "ecl" (fn [s]
           (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s)) ; (Eye Color)
   "pid" (fn [s]
           (re-matches #"[0-9]{9,9}" s)) ; (Passport ID)
   ;"cid" ; (Country ID)
   })

(defn valid-passport-data?
  [passport]
  (every? (fn [[field data]]
            (if-let [spec-fn (passport-spec field)]
              (try
                (spec-fn data)
                (catch Exception _
                  false))
              true ;; by default, unknown fields are fine
              ))
          passport))

(defrun part2
  (->> input
       (filter valid-passport-structure?)
       (filter valid-passport-data?)
       (count)))
