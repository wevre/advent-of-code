(ns advent-of-code.2020.day-7
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]
            [clojure.string :as str]))

;; --- Day 7: Handy Haversacks ---

(defn parse-spec [spec]
  (->> (str/split spec #" " 2)
       ((juxt second (comp #(Integer/parseInt %) first)))))

(defn parse-rule [s]
  (let [[head body] (str/split s #" bags contain ")
        specs (str/split body #" bags?[,.] ?")]
    {:container head
     :contents (into {} (comp (remove #{"no other"}) (map parse-spec)) specs)})
  )

(defn expand-rule [{:keys [container contents]}]
  (map vector (repeat container) contents))

(pldb/db-rel can-contain ^:index out ^:index inn n)

(defn can-contain-transitive [out inn n]
  (l/conde
   [(can-contain out inn n)]
   [(l/fresh [mid o p]
      (can-contain out mid o)
      (can-contain-transitive mid inn p)
      (fd/eq (= (* o p) n))
      #_(fd/* o p n))]))

(defn bags-contained-in [needle facts]
  (pldb/with-db facts
    (l/run* [q]
      (l/fresh [x y]
        (can-contain-transitive x needle y)
        (l/== q [x y])))))

(defn ingest-rule [db [container contents]]
  (apply pldb/db-fact db can-contain container contents))

(defn ingest [rules]
  (reduce ingest-rule (pldb/db) rules))

(defn puzzle1 [bag rules]
  (->> rules
       str/split-lines
       (map parse-rule)
       #_#_#_#_#_#_
       (mapcat expand-rule)
       ingest
       (bags-contained-in bag)
       (map first)
       set
       count
       ))

(comment
  (puzzle1 "shiny gold" (slurp "input/2020/7-bags.txt")))

(defn bags-that-contain [needle facts]
  (pldb/with-db facts
    (l/run* [q]
      (l/fresh [x y]
        (can-contain-transitive needle x y)
        (l/== q [x y])))))


(defn puzzle2 [bag rules]
  (->> rules
       str/split-lines
       (map parse-rule)
       (mapcat expand-rule)
       ingest
       (bags-that-contain bag)
       (map second)
       (apply +)))

(comment
  (puzzle2 "shiny gold" input))