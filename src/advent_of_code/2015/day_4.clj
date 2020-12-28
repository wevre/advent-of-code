(ns advent-of-code.2015.day-4
  (:require [digest :refer [md5]]
            [clojure.string :as str]))

(defn puzzle1 [secret test]
  (->> (map str (repeat secret) (drop 1 (range)))
       (map md5)
       (take-while #(not (str/starts-with? % test)))
       count
       inc))

(comment
  (let [secret "abcdef"] (puzzle1 secret "00000"))
  (let [secret "pqrstuv"] (puzzle1 secret "00000"))
  
  (puzzle1 "ckczppom" "00000")
  
  (puzzle1 "ckczppom" "000000"))
