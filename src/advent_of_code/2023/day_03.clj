(ns advent-of-code.2023.day-03
  (:require [advent-of-code.common :refer [locmap<-]]))

(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn ?symbol [v] (and (not= v \.) (not (digits v))))

(defn symbols<- [{:keys [locmap]}]
  (into {} (filter #(?symbol (second %))) locmap))

(defn neighbors [[r c]]
  (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)]
    [(+ r dr) (+ c dc)]))

(defn adjacent-digits [locmap sloc]
  (for [nloc (neighbors sloc)
        :let [c (locmap nloc)]
        :when (digits c)]
    nloc))

(defn non-digit [locmap n c's r]
  (or
   (->> c's
        (map (fn [c] [r c]))
        (map (juxt identity locmap))
        (drop-while (fn [[_loc v]] (digits v)))
        first)
   [[r n] nil]))

(defn part-number [locmap n [r c]]
  (let [[[_ sc] _] (non-digit locmap 0 (for [i (range)] (- c 1 i)) r)
        [[_ ec] _] (non-digit locmap n (for [i (range)] (+ c 1 i)) r)
        dig's (for [c (range (inc sc) ec)] (locmap [r c]))]
    [[r (inc sc)] (parse-long (apply str dig's))]))

(defn part's [{:keys [locmap] [_row's col's] :size} loc]
  (into {}
        (map #(part-number locmap col's %))
        (adjacent-digits locmap loc)))

(defn parse [schematic]
  (reduce (fn [acc [k v]] (conj acc {:v v :parts (part's schematic k)}))
          []
          (symbols<- schematic)))

(defn schematic<- [input]
  (locmap<- (slurp input)))

(comment
  (def schematic (schematic<- "input/2023/03-schematic.txt"))
  (def schematic (schematic<- "input/2023/03-sample-schematic.txt"))

  ;; iterate through the map and if it is not a digit or . then it's a symbol
  ;; Look at the symbol's neighbors and

  ;; year 2023 day 03 puzzle 1
  (->> (parse schematic)
       (map :parts)
       (into {})
       vals
       (reduce +))

  ;; year 2023 day 03 puzzle 2
  (->> (parse schematic)
       (filter (fn [{:keys [v]}] (= v \*)))
       (map :parts)
       (filter #(= 2 (count %)))
       (map vals)
       (map #(apply * %))
       (reduce +)))
