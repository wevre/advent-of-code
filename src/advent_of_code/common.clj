(ns advent-of-code.common
  (:require [clojure.string :as str]
            [clojure.walk :as walk])
  (:import [clojure.lang PersistentQueue]))

(defn locmap<-digits
  ([s] (locmap<-digits s identity))
  ([s f]
   (let [lmap
         (->> s
              (str/split-lines)
              (map-indexed vector)
              (mapcat (fn [[r l]]
                        (map-indexed (fn [c v]
                                       [[r c] (f (Character/digit v 10))]) l)))
              (into {}))
         size (reduce (fn [[rm cm] [r c]] [(max rm (inc r)) (max cm (inc c))])
                      [0 0]
                      (keys lmap))]
     {:locmap lmap :size size})))

(defn locmap<-
  ([s] (locmap<- identity s))
  ([f s]
   (let [lmap
         (->> s
              (str/split-lines)
              (map-indexed vector)
              (mapcat (fn [[r l]] (map-indexed (fn [c v] [[r c] (f v)]) l)))
              (into {}))
         size (reduce (fn [[rm cm] [r c]] [(max rm (inc r)) (max cm (inc c))])
                      [0 0]
                      (keys lmap))]
     {:locmap lmap :size size})))

(defn parse-longs [s]
  (map parse-long (re-seq #"-?\d+" s)))

(defn split-long-lines [s]
  (map parse-longs (str/split-lines s)))

(defn split-grouped-lines [input]
  (->> input
       str/split-lines
       (partition-by empty?)
       (take-nth 2)))

(defn transpose
  ([ll] (transpose nil ll))
  ([pad ll]
   (->> ll
        (map #(concat % (repeat ::end)))
        (apply map vector)
        (take-while #(not-every? #{::end} %))
        (walk/postwalk-replace {::end pad}))))

(comment
  (transpose \space [[1 2 3] [4 5 ] [7 8 9]]))

(defn range-inc
  ([end] (range (inc end)))
  ([start end] (range start (inc end)))
  ([start end step] (range start (inc end) step)))

(defn range-x
  "Return range from `start` to `end`, inclusive. `end` need not be greater than
   `start`. Adapted from zelark (so it works even if start == end)."
  ([end] (range-inc 0 end))
  ([start end]
   (cond
     (< start end) (range start (inc end))
     (< end start) (range start (dec end) -1)
     :else (repeat start))))

(defn points-along [[x1 y1] [x2 y2]]
  (map vector (range-x x1 x2) (range-x y1 y2)))

(defn z-combinator
  "Takes function `g` of n+1 parameters, the first being a memoized function to
   be called recursively with the other n.

   ```
   (defn my-memoized-recursive-fn [args to close over]
     (z-combinator
      (fn [f arg1 ... argn]
        ...
        (f val1 ... valn)))
   ```
  "
  [g]
  (let [fix (fn [f] (fn z [& args] (apply f z args)))]
    (fix (memoize g))))

;; another handy gem from @zelark
(defn queue [& args] (into PersistentQueue/EMPTY args))
