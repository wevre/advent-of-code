(ns advent-of-code.2020.day-19
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; --- Day 19: Monster Messages ---

(defn parse 
  "Returns a list, ready to pour into a map, of entries [id <map>], where map is
   either {:char <single-character>} or {:sub-rules <set-of-sub-rules>}."
  [line]
  (let [[id rest] (str/split line #":")]
    [(edn/read-string id) 
     (if (some #{\"} rest)
       {:char (first (edn/read-string rest))}
       {:sub-rules (edn/read-string
                    (str "#{(" (str/replace rest "|" ")(") ")}"))})]))

(defn match [rules]
  (fn match* [[id & ids] s]
    (let [{:keys [char sub-rules]} (rules id)]
      (cond
        char (when (= char (first s)) (recur ids (rest s)))
        sub-rules (some #(match* (concat % ids) s) sub-rules)
        (empty? s) true))))

(defn puzzle [patch input]
  (let [[rules messages] (str/split input #"\n\n")
        messages (str/split-lines messages)
        rules (into {} (map parse) (concat (str/split-lines rules) patch))]
    (count (filter some? (map (partial (match rules) (list 0)) messages)))))

(comment
  (puzzle () (slurp "input/2020/19-messages.txt"))

  (puzzle '("8: 42 | 42 8" "11: 42 31 | 42 11 31")
          (slurp "input/2020/19-messages.txt"))

  (let [input "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb"]
    (puzzle () input)))
