(ns advent-of-code.common.file
  (:require [clojure.string :as str]))

(defn split-grouped-lines [input]
  (-> input
      str/split-lines
      (->>
       (partition-by empty?)
       (take-nth 2))))

(comment
  (split-grouped-lines (slurp "input/2022/05-cranes.txt")))