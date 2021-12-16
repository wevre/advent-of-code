(ns advent-of-code.2021.day-16
  "I chose not to have a global atom to help consume the bits. Instead, all my
   parse functions take bits and return a vector `[result bits]`. Consistently,
   then, all calls to these functions destructure the results with this pattern:
       (defn parse-zzz [bits]
         (let [[result bits] (parse-xxx bits)
               [other bits] (parse-yyy bits)]
           ...do something...
           [result bits])
   I normally don't like clobbering vars with repeating let bindings, but here
   the pattern is useful and consistent.
   To evaluate the results, I use clojure.walk/postwalk which is one of those
   'magical' functions that I love to use. And first time for me, I used
   clojure.core.match/match to identify the particular subforms (during the
   walk) that need to be evaluated."
  (:require [clojure.walk :refer [postwalk]]
            [clojure.core.match :refer [match]]))

;; --- Day 16: Packet Decoder ---
;; https://adventofcode.com/2021/day/16

(def bin<-hex (-> {\0 "0000", \1 "0001", \2 "0010", \3 "0011"
                   \4 "0100", \5 "0101", \6 "0110", \7 "0111"
                   \8 "1000", \9 "1001", \A "1010", \B "1011"
                   \C "1100", \D "1101", \E "1110", \F "1111"}
                  (update-vals seq)))

(defn parse-input [s]
  (mapcat #(bin<-hex %) s))

(defn num<-bits [bits] (Long/parseLong (apply str bits) 2))

(declare parse-packet)

(defn parse-header [bits]
  (let [[ver bits] (split-at 3 bits)
        [typ bits] (split-at 3 bits)]
    [{:ver (num<-bits ver) :typ (num<-bits typ)} bits]))

(defn parse-literal [bits]
  (loop [bits bits val ()]
    (let [[[pre & grp] bits] (split-at 5 bits)
          val (concat val grp)]
      (if (= \0 pre)
        [{:val (num<-bits val)} bits]
        (recur bits val)))))

(defn parse-n-packets [n bits]
  (loop [subs [] bits bits n n]
    (if (zero? n)
      [{:subs subs} bits]
      (let [[packet bits] (parse-packet bits)]
        (recur (conj subs packet) bits (dec n))))))

(defn parse-n-bits [n bits]
  (let [[bits rest] (split-at n bits)]
    (loop [subs [] bits bits]
      (if (seq bits)
        (let [[packet bits] (parse-packet bits)]
          (recur (conj subs packet) bits))
        [{:subs subs} rest]))))

(defn parse-operator [bits]
  (let [[typ & bits] bits
        [len bits] (split-at (if (= \0 typ) 15 11) bits)
        len (num<-bits len)]
    (if (= \0 typ)
      (parse-n-bits len bits)
      (parse-n-packets len bits))))

(defn parse-packet [bits]
  (let [[{:keys [typ] :as header} bits] (parse-header bits)
        [packet bits] ((if (= 4 typ) parse-literal parse-operator) bits)]
    [(merge header packet) bits]))

(defn extract-ver [x]
  (let [Long Long]
    (match [x]
      [[(Long :<< type) & _]] (apply + x)
      [{:ver v :subs s}] (+ v s)
      [{:ver v}] v
      :else x)))

(def fn<-op {0 +,
             1 *,
             2 min,
             3 max,
             5 #(if (apply > %&) 1 0),
             6 #(if (apply < %&) 1 0),
             7 #(if (apply = %&) 1 0)})

(defn eval-ops [x]
  (match [x]
    [{:typ 4}] (:val x)
    [{:typ t :subs s}] (apply (fn<-op t) s)
    :else x)
  )

(comment
  ;; puzzle 1
  (let [[packet _bits] (parse-packet (parse-input (slurp "input/2021/16-packets.txt")))]
    (postwalk extract-ver packet))

  ;; puzzle 2
  (let [[packet _bits] (parse-packet (parse-input (slurp "input/2021/16-packets.txt")))]
    (postwalk eval-ops packet))
  )
