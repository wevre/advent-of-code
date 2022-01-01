(ns advent-of-code.2016.day-05
  (:require [clj-commons.digest :refer [md5]]
            [clojure.string :as str]))

;;;; --- Day 5: How About a Nice Game of Chess? ---
;;;; https://adventofcode.com/2016/day/5

(def input "reyedfim")

(def interesting (->> (map #(md5 (str input %)) (drop 1 (range)))
                      (filter #(str/starts-with? % "00000"))))

(defn update-pswd [pswd [_ _ _ _ _ n v]]
  (let [i (- (int n) (int \0))]
    (cond-> pswd (and (<= 0 i 7) (not (pswd i))) (assoc i v))))

(comment
  ;; part 1 -- 11s
  (time
   (->> interesting
        (take 8)
        (map #(subs % 5 6))
        (apply str)))   ;=> "f97c354d"

  ;; part 2  -- 41s
  (time
   (->> interesting
        (reduce (fn [pswd hash]
                  (let [pswd (update-pswd pswd hash)]
                    (if (not-any? nil? pswd)
                      (reduced pswd)
                      pswd)))
                [nil nil nil nil nil nil nil nil])
        (apply str)))   ;=> "863dde27"
  )
