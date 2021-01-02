(ns advent-of-code.2015.day-12
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :as walk]))

(defn puzzle1 [input]
  (->> (str/escape input {\: \ })
       edn/read-string
       (walk/postwalk #(if (map? %) (seq %) %))
       flatten
       (filter number?)
       (reduce +)))

(defn puzzle2 [input]
  (->> (str/escape input {\: \ })
       edn/read-string
       (walk/postwalk 
        #(if (map? %) (when (not-any? #{"red"} (vals %)) (seq %)) %))
       flatten
       (filter number?)
       (reduce +)))

(comment
  (puzzle2 (slurp "input/2015/12-json.txt"))
  (puzzle1 (slurp "input/2015/12-json.txt")))
