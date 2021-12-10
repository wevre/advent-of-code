(ns advent-of-code.2021.day-8
  (:require [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

;; --- Day 8: Seven Segment Search ---

(comment
  ;; puzzle1: find how many "easy" numbers are in the output of each entry
  (->> (slurp "input/2021/8-digits.txt")
       (re-seq #"[a-g]+")
       (partition 14)        ;; divide into entries
       (map #(drop 10 %))    ;; keep only the "output" portion
       (mapcat #(map count %))
       (filter #{2 4 3 7})   ;; keep only the "easy" number of segments
       count)   ;;=> 1000
  )

(def easy "map from (unique) number of segments to known digit"
  {2 1, 4 4, 3 7, 7 8})

(defn resolve-easy [freq-map known]
  (let [easy-segs (set (keys easy))]
    (reduce-kv (fn [m k v]
                 (cond-> m (easy-segs k) (assoc (easy k) (first v))))
               known
               freq-map)))

(defn resolve-six-segs [freq-map known]
  (let [four (nth known 4)
        one (nth known 1)]
    (reduce (fn [m v]
              (cond
                (set/subset? four v) (assoc m 9 v)
                (set/subset? one v) (assoc m 0 v)
                :else (assoc m 6 v)))
            known
            (freq-map 6))))

(defn resolve-five-segs [freq-map known]
  (let [one (nth known 1)
        nine (nth known 9)]
    (reduce (fn [m v]
              (cond
                (set/subset? one v) (assoc m 3 v)
                (set/subset? v nine) (assoc m 5 v)
                :else (assoc m 2 v)))
            known
            (freq-map 5))))

(defn num<-digits [digits]
  (->> digits
       (map str)
       (apply str)
       Integer/parseInt))

(comment
  (num<-digits '(1 2 3 4)))

(defn resolve-entry [entry]
  (let [input (group-by count (take 10 entry))
        output (drop 10 entry)
        known (->> (into [] (repeat 10 nil))
                   (resolve-easy input)
                   (resolve-six-segs input)
                   (resolve-five-segs input))
        lookup (reduce-kv (fn [m k v] (assoc m v k)) {} known)]
    (->> output
         (map lookup)
         num<-digits)
    ))

(defn puzzle2 [file]
  (->> (slurp file)
       (re-seq #"[a-g]+")
       (map set)
       (partition 14)
       (map resolve-entry)
       (apply +)))

(comment
    (pprint (puzzle2 "input/2021/8-digits.txt"))
)
