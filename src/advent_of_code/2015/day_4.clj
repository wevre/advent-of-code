(ns advent-of-code.2015.day-4
  (:require [digest :refer [md5]]
            [clojure.string :as str]))

;; --- Day 4: The Ideal Stocking Stuffer ---

(defn puzzle [pre secret]
  (->> (map str (repeat secret) (drop 1 (range)))
       (map md5)
       (reduce (fn [r v] (cond-> (inc r) (str/starts-with? v pre) reduced)) 0)))

(comment
  (let [secret "abcdef"] (puzzle "00000" secret))
  (let [secret "pqrstuv"] (puzzle "00000" secret))
  
  (puzzle "00000" "ckczppom")
  
  (puzzle "000000" "ckczppom"))
