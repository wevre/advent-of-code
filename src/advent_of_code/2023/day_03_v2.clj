(ns advent-of-code.2023.day-03-v2
  (:require [clojure.string :as str]))

(def ?digit #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn ?symbol [v] (and (not (#{\. \newline} v)) (not (?digit v))))

(defn neighbors [[r c]]
  (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)] [(+ r dr) (+ c dc)]))

(defn add-part [parts digit-stack]
  (let [num (parse-long (str/join (map second digit-stack)))]
    (into parts (map (fn [[loc _v]] [loc num])) digit-stack)))

(defn conjoin [{:as schematic :keys [digit-stack]} v loc]
  (if (?digit v)
    (update schematic :digit-stack conj [loc v])
    (-> schematic
        (cond-> (?symbol v) (update :symbols assoc loc v))
        (update :parts add-part digit-stack)
        (assoc :digit-stack []))))

(defn schematic<- [input]
  (loop [[v & v's] input [r c :as loc] [0 0] schematic {:symbols {} :parts {} :digit-stack []}]
    (if-not v
      schematic
      (recur
       v's
       (if (#{\newline} v) [(inc r) 0] [r (inc c)])
       (conjoin schematic v loc)))))

(defn stitch [{:keys [symbols parts]} pred]
  (for [[loc v] symbols :when (pred v)]
    (vec (into #{} (keep parts) (neighbors loc)))))

(comment
  (def schematic (schematic<- (slurp "input/2023/03-sample-schematic.txt")))
  (def schematic (schematic<- (slurp "input/2023/03-schematic.txt")))

  ;; year 2023 day 03 puzzle 1
  (->> (stitch schematic ?symbol)
       flatten
       (reduce +))   ;; => 538046

  ;; year 2023 day 03 puzzle 2
  (->> (stitch schematic #{\*})
       (filter #(= 2 (count %)))
       (map #(apply * %))
       (reduce +))   ;; => 81709807
  )