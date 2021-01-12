(ns advent-of-code.2015.day-12
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :as walk]))

;; --- Day 12: JSAbacusFramework.io ---

(defn puzzle [de-map input]
  (->> (str/escape input {\: \ })
       edn/read-string
       (walk/postwalk #(if (map? %) (de-map %) %))
       flatten
       (filter number?)
       (reduce +)))

(comment
  (let [input (slurp "input/2015/12-json.txt")]
    [(puzzle #(seq %) input)
     (puzzle #(when (not-any? #{"red"} (vals %)) (seq %)) input)]))
