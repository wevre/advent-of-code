(ns advent-of-code.2023.day-15-tokens
  (:require [clojure.string :as str]))

(defn HASH [t]
  (reduce (fn [acc c] (rem (* 17 (+ acc (int c))) 256)) 0 t))

(defn upsert-lens [label flen]
  (fn [rf]
    (let [?replaced (volatile! false)]
      (fn
        ([] (rf))
        ([result] (when-not @?replaced (rf result [label flen])) (rf result))
        ([result [l _fl :as input]]
         (if (= label l)
           (do (vreset! ?replaced true) (rf result [label flen]))
           (rf result input)))))))

(defn insert-lens [contents label flen]
  (sequence (upsert-lens label flen) contents))

(defn remove-lens [contents label]
  (remove (fn [[l _]] (= label l)) contents))

(defn HASHMAP [step's]
  (reduce (fn [boxes step]
            (let [[label flen] (str/split step #"=|-")
                  box-no (HASH label)]
              (if flen
                (update boxes box-no insert-lens label (parse-long flen))
                (update boxes box-no remove-lens label))))
          {}
          step's))

(defn focusing-power [[box-no contents]]
  (map-indexed (fn [i [_l fl]] (* (inc box-no) (inc i) fl)) contents))

(comment
  (def input (str/split (slurp "input/2023/15-tokens.txt") #","))
  (def input (str/split "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" #","))

  ;; year 2023 day 15 puzzle 1
  (transduce (map HASH) + input)
  ;; => 510792

  ;; year 2023 day 15 puzzle 2
  (transduce (mapcat focusing-power) + (HASHMAP input))
  ;; => 269410
  )
