(ns advent-of-code.2023.day-03
  (:require [advent-of-code.common :refer [locmap<-]]))

(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn ?symbol [v] (and (not= v \.) (not (digits v))))

(defn symbols<- [locmap]
  (into {} (filter #(?symbol (second %))) locmap))

(defn neighbors [[r c]]
  (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)]
    [(+ r dr) (+ c dc)]))

(defn adjacent-digits [locmap sloc]
  (for [nloc (neighbors sloc) :when (digits (locmap nloc))] nloc))

(defn non-digit
  "Starting at location [r c] increment or decrement c (based on op) until we
   encounter something not a digit and return c. If we run off the edge of the
   map return n, which will be (depending on op) the outermost valid c."
  [locmap r c op n]
  (or
   (->> (range)
        (map (fn [i] [r (op c 1 i)]))
        (drop-while (fn [loc] (digits (locmap loc))))
        first
        second)
   n))

(defn part-number [locmap n [r c]]
  (let [sc (non-digit locmap r c - 0)
        ec (non-digit locmap r c + n)
        dig's (for [c (range (inc sc) ec)] (locmap [r c]))]
    [[r (inc sc)] (parse-long (apply str dig's))]))

(defn part's [locmap col's loc]
  (into {}
        (map #(part-number locmap col's %))
        (adjacent-digits locmap loc)))

(defn schematic<-
  "Returns a list of symbols-maps, each like so:
   {:v <symbol> :parts <adjacent-part-numbers>}.

   The part numbers are also a map, keyed by the loc of the part number's first
   digit. That loc isn't really important, it just provides a unique key,
   since part numbers themselves may not be unique(?).

   For the sample data, the parsed schematic is:
   [{:v *, :parts {[4 0] 617}}
    {:v $, :parts {[9 1] 664}}
    {:v *, :parts {[0 0] 467, [2 2] 35}}
    {:v *, :parts {[7 6] 755, [9 5] 598}}
    {:v +, :parts {[6 2] 592}}
    {:v #, :parts {[2 6] 633}}]

   With this result in hand, it is easy to complete the two puzzles."
  [input]
  (let [{:keys [locmap] [_row's col's] :size} (locmap<- (slurp input))]
    (into []
          (map (fn [[k v]] {:v v :parts (part's locmap col's k)}))
          (symbols<- locmap))))

(comment
  (def schematic (schematic<- "input/2023/03-schematic.txt"))
  (def schematic (schematic<- "input/2023/03-sample-schematic.txt"))

  ;; year 2023 day 03 puzzle 1
  (->> schematic
       (map :parts)
       (into {})
       vals
       (reduce +))   ;; => 538046


  ;; year 2023 day 03 puzzle 2
  (->> schematic
       (filter (fn [{:keys [v]}] (= v \*)))
       (map :parts)
       (filter #(= 2 (count %)))
       (map vals)
       (map #(apply * %))
       (reduce +))   ;; => 81709807
)
