(ns advent-of-code.2016.day-04
  (:require [clojure.string :as str]))

;;;; --- Day 4: Security Through Obscurity ---
;;;; https://adventofcode.com/2016/day/4

(defn parse-input [s]
  (for [line (str/split-lines s)]
    (let [[_ room sector check]
          (re-matches #"([^\d]*)-(\d+)\[([a-z]+)\]" line)]
      {:room room
       :sector (parse-long sector)
       :check check})))

(defn checksum [room]
  (->> (frequencies (remove #{\-} room))
       (reduce-kv (fn [m k v]
                    (update m v (fnil conj (sorted-set)) k))
                  (sorted-map-by >))
       vals
       (apply concat)
       (take 5)
       (apply str)))

(defn roll-char [c x]
  (char (+ 97 (mod (+ (int c) x -97) 26))))

(defn decrypt [{:keys [room sector _check] :as m}]
  (let [decrypted (->> room
                       (map #(if (= \- %) \  (roll-char % sector)))
                       (apply str))]
    (assoc m :room decrypted)))

(defn real? [{:keys [room _sector check]}]
  (= check (checksum room)))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2016/04-rooms.txt"))]
    (->> input
         (filter real?)
         (map :sector)
         (reduce +)))   ;=> 173787

  ;; part 2
  (let [input (parse-input (slurp "input/2016/04-rooms.txt"))]
    (->> input
         (filter real?)
         (map decrypt)
         (filter #(re-seq #"north" (:room %)))
         first
         :sector))   ;=> 548
  )
