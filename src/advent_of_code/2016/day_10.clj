(ns advent-of-code.2016.day-10
  (:require [clojure.string :as str]))

;;;; --- Day 10: Balance Bots ---
;;;; https://adventofcode.com/2016/day/10

(def sorted-conj (comp vec sort (fnil conj [])))

(defn parse-input [s]
  (reduce
   (fn [m line]
     (let [[a b c d e f] (re-seq #"output|bot|\d+" line)]
       (if f
         (let [key [:bot (parse-long b)]
               low [(keyword c) (parse-long d)]
               high [(keyword e) (parse-long f)]]
           (assoc-in m [:rules key] [low high]))
         (let [key [:bot (parse-long c)]]
           (update-in m [:values key] sorted-conj (parse-long a))))))
   {}
   (str/split-lines s)))

(defn proceed [rules]
  (fn [values]
    (reduce-kv
     (fn [m [ob _num :as k] [lo hi :as _v]]
       (if (and (= ob :bot) hi)
         (-> m
             (update ((rules k) 0) sorted-conj lo)
             (update ((rules k) 1) sorted-conj hi)
             (dissoc k))
         (update m k sorted-conj lo)))
     {}
     values)))

(defn find-bot [targ]
  (fn [values]
    (some (fn [[[ob n] v]] (when (and (= ob :bot) (= targ v)) n)) values)))

(comment
  ;; part 1
  (let [{:keys [rules values]} (parse-input (slurp "input/2016/10-bots.txt"))]
    (->> (iterate (proceed rules) values)
         (keep (find-bot [17 61]))
         first))   ;=> 118

  ;; part 2
  (let [{:keys [rules values]} (parse-input (slurp "input/2016/10-bots.txt"))]
    (->> (iterate (proceed rules) values)
         (map #(select-keys % [[:output 0] [:output 1] [:output 2]]))
         (filter #(= 3 (count %)))
         first
         (mapcat val)
         (reduce *)))   ;=> 143153
  )
