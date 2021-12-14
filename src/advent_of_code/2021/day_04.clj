(ns advent-of-code.2021.day-04
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]))

;; --- Day 4: Giant Squid ---

;; My first thought on this was to determine how many turns until a board won,
;; and then run that function on all the boards sort them, and return the one
;; that won the quickest. Then I did a different approach. But after I saw part
;; two, I realized my original idea would have worked best for both parts.

;; I'm not interested in modeling a bingo board as a 2D structure. I just need
;; to be able to find a number within the board, and keep track of "called"
;; numbers. I used a hash-map here, but could it be done with a set?

(defn parse-board [input]
  (let [board (into [] (map edn/read-string (re-seq #"\d+" input)))]
    {:board board
     :marks (into [] (repeat 25 0))
     :idx (reduce-kv (fn [m k v] (assoc m v k)) {} board)
     :win? false}))

(def runs [[0 1 2 3 4]
           [5 6 7 8 9]
           [10 11 12 13 14]
           [15 16 17 18 19]
           [20 21 22 23 24]
           [0 5 10 15 20]
           [1 6 11 16 21]
           [2 7 12 17 22]
           [3 8 13 18 23]
           [4 9 14 19 24]])   ;; Probably a way to generate this programatically.

(defn all-ones? [run] (apply = (conj run 1)))

(defn winner? [marks]
  (->> runs
       (map #(map marks %))
       (keep all-ones?)
       (some true?)))

(defn mark-winner [board]
  (assoc board :win? (winner? (:marks board))))

(defn play-number [n board]
  ;; if n is present on the board, then put a 1 in marked.
  (if-let [i (get-in board [:idx n])]
    (-> board (assoc-in [:marks i] 1) mark-winner)
    board))

(defn sum-unmarked [{:keys [board marks]}]
  (apply + (map * board (map #(- 1 %) marks))))

(defn puzzle1 [input]
  (let [[nums & boards] (str/split input #"\n\n")
        nums (map edn/read-string (re-seq #"\d+" nums))
        boards (map parse-board boards)]
    (loop [[n & ns] nums boards boards]
      (let [boards (map #(play-number n %) boards)]
        (if-let [winner (first (filter :win? boards))]
          (* n (sum-unmarked winner))
          (recur ns boards))))))

(defn puzzle2 [input]
  (let [[nums & boards] (str/split input #"\n\n")
        nums (map edn/read-string (re-seq #"\d+" nums))
        boards (map parse-board boards)]
    (loop [[n & ns] nums boards boards]
      (let [boards (map #(play-number n %) boards)]
        (if (and (= 1 (count boards)) (:win? (first boards)))
          (* n (sum-unmarked (first boards)))
          (recur ns (filter #(not (:win? %)) boards)))))))

(comment

  (puzzle1 (slurp "input/2021/4-bingo.txt"))

  (puzzle2 (slurp "input/2021/4-bingo.txt"))

  (def sample {:board [14 21 17 24 4 10 16 15 9 19 18 8 23 26 20 22 11 13 6 5 2 0 12 3 7]
               :marks [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0],
               :idx {0 21, 7 24, 20 14, 24 3, 4 4, 15 7, 21 1, 13 17, 22 15, 6 18, 17 2, 3 23, 12 22, 2 20, 23 12, 19 9, 11 16, 9 8, 5 19, 14 0, 26 13, 16 6, 10 5, 18 10, 8 11},
               :win? false})

  (->> sample
       (play-number 8)
       (play-number 16)
       (play-number 0)
       (play-number 11)
       (play-number 21))
  )