(ns advent-of-code.2022.day-15-beacons
  (:require [clojure.string :as str]
            [advent-of-code.common :as common]
            [clojure.java.math :as math]))

;; 2022-12-15 00:24
;;    This code isn't very inspiring. I'm too tired to be more clever here. My
;;    first attempt at Part 2 took over 10 minutes to run. Then I tried pmap and
;;    got it down to about 2.5 minutes. Still seems slow.
;; 2022-12-15 00:37
;;    I have an idea of a different way to do this, but I'll have to attempt it
;;    after some sleep.

(defn parse [input]
  (->> input
       str/split-lines
       (map common/parse-longs)))

(defn overlap? [a b c d]
  (and (<= c (inc b)) (>= d (dec a))))

(defn combine [a b c d]
  (when (overlap? a b c d)
    [(min a c) (max b d)]))

(defn no-beacons [[sx sy bx by] y-targ]
  (let [mdist (+ (math/abs (- bx sx)) (math/abs (- by sy)))
        vdist (math/abs (- y-targ sy))
        hwid (- mdist vdist)]
    (when (pos? hwid) [(- sx hwid) (+ sx hwid)])))

(defn merge-spans [spans]
  (reduce (fn [acc [minx maxx]]
            (loop [[[a b :as n] & r] acc out [] minx minx maxx maxx]
              (if-not n
                (conj out [minx maxx])
                (if-let [[minx maxx] (combine a b minx maxx)]
                  (recur r out minx maxx)
                  (recur r (conj out n) minx maxx)))))
          []
          spans))

(def ^:dynamic *targ-row* 2000000)

(def ^:dynamic *input* "input/2022/15-beacons.txt")

(def ^:dynamic *max-coord* 4000000)

(defn find-gaps [parsed max-x]
  (fn [y]
    (let [spans (->> parsed (keep #(no-beacons % y)) merge-spans)]
      (when (some (fn [[lo hi]] (not (<= lo 0 max-x hi))) spans)
        [y spans]))))

(comment
  ;; puzzle 1
  (binding [*targ-row* *targ-row* #_10
            *input* *input* #_"input/2022/15-beacons-sample.txt"]
    (let [parsed (parse (slurp *input*))
          spans (->> parsed
                     (keep (fn [data] (no-beacons data *targ-row*)))
                     merge-spans)
          beacons (->> parsed
                       (keep (fn [[_ _ bx by]]
                               (when (some (fn [[lo hi]]
                                             (and (= by *targ-row*)
                                                  (<= lo bx hi)))
                                           spans)
                                 [bx by])))
                       distinct)
          lengths (->> spans (map (fn [[a b]] (+ 1 (- b a)))) (apply +))]
      (- lengths (count beacons))))   ; => 6124805

  ;; puzzle 2 -- 149s
  (binding [*input* *input* #_"input/2022/15-beacons-sample.txt"
            *max-coord* *max-coord* #_ 20]
    (time
     (let [parsed (parse (slurp *input*))
           gap-finder (find-gaps parsed *max-coord*)
           found (->> (range 0 (inc *max-coord*))
                      (pmap gap-finder)
                      (filter identity))
           [y [[_ b] [_ _]]] (first found)]
       (+ y (* 4000000 (inc b))))))   ; => 12555527364986
  )
