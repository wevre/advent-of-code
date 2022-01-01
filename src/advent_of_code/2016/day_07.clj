(ns advent-of-code.2016.day-07
  (:require [clojure.string :as str]))

;;;; --- Day 7: Internet Protocol Version 7 ---
;;;; https://adventofcode.com/2016/day/7

(defn parse-input [s]
  (for [line (str/split-lines s)]
    (str/split line #"\[|\]")))

(defn has-abba? [s]
  (if-let [[_ a b] (re-find #"([a-z])([a-z])\2\1" s)]
    (not= a b)
    false))

(defn supports-tls? [segs]
  (let [outside (take-nth 2 segs)
        inside (take-nth 2 (drop 1 segs))]
    (and (some has-abba? outside)
         (not-any? has-abba? inside))))

(defn get-aba's [seg]
  (for [[a b c] (partition 3 1 seg)
        :when (and (= a c) (not= a b))]
    [a b]))

(defn supports-ssl? [segs]
  (let [outside (take-nth 2 segs)
        inside (take-nth 2 (drop 1 segs))
        ABA's (mapcat get-aba's outside)
        BAB's (mapcat get-aba's inside)]
    (some (set ABA's) (map reverse BAB's))))

(comment
  ;; part 1
  (->> (parse-input (slurp "input/2016/07-addresses.txt"))
       (filter supports-tls?)
       count)   ;=> 118

  ;; part 2
  (->> (parse-input (slurp "input/2016/07-addresses.txt"))
       (filter supports-ssl?)
       count)   ;=> 260
  )
