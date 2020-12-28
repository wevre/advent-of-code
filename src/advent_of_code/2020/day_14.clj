(ns advent-of-code.2020.day-14
  (:require [clojure.string :as str]))

(defn decode [mask]
  (letfn [(step [acc l [c & cs]]
                (case c
                  \0 (recur acc (conj l \X) cs)
                  \1 (recur acc (conj l \1) cs)
                  \X (recur (step acc (conj l \0) cs) (conj l \1) cs)
                  nil (conj acc l)))]
    (map str/join (step [] [] mask))))

(defn mask-parts [bitmap]
  [(Long/parseLong (str/escape bitmap {\X \0}) 2)            ; OR-mask
   (Long/parseLong (str/escape bitmap {\1 \0, \X \1}) 2)])   ; AND-mask

(defn apply-mask [[OR-mask AND-mask] v]
  (bit-or OR-mask (bit-and AND-mask (Long/parseLong v))))

(defn reduce-mask-val [state [_ mask i v]]
  (if mask
    (assoc state :mask (mask-parts mask))
    (assoc-in state [:mem i] (apply-mask (:mask state) v))))

(defn reduce-mask-loc [state [_ mask i v]]
  (if mask
    (assoc state :mask (map mask-parts (decode mask)))
    (reduce #(assoc-in %1 [:mem (apply-mask %2 i)] (Long/parseLong v)) 
            state (:mask state))))

(defn puzzle [rf in]
  (->> (re-seq #"(?:mask = (\w+))|(?:(\d+)\] = (\d+))" in)
       (reduce rf {:mask [] :mem {}})
       :mem
       vals
       (reduce +)))

(comment
  (puzzle reduce-mask-val (slurp "input/2020/14-bit_masks.txt"))
  
  (puzzle reduce-mask-loc (slurp "input/2020/14-bit_masks.txt"))
  
  (let [input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"]
    (puzzle reduce-mask-val input))
  
  (let [input "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"]
    (puzzle reduce-mask-loc input))
  )