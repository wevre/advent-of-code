(ns advent-of-code.2015.day-7
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]
            [clojure.string :as str]))

;; Okay, may not be able to do this core.logic, or at least I don't
;; know how to write the goals to test for bit-wise AND and OR.
;; 
;; But I could write it with `delay`s, as demonstrated here:
;; https://blog.michielborkent.nl/blog/2017/10/10/parsing-a-circuit-with-clojure-spec/




(pldb/db-rel signal ^:index wire n)
(pldb/db-rel and-gate ^:index left ^:index right ^:index wire)
(pldb/db-rel or-gate ^:index left ^:index right ^:index wire)
(pldb/db-rel left-shift ^:index input b ^:index wire)
(pldb/db-rel right-shift ^:index input b ^:index wire)
(pldb/db-rel not-gate ^:index input ^:index wire)

(defn has-signal [wire n]
  (l/conde
   [(signal wire n)]
   [(l/fresh [left l right r]
      (and-gate left right wire)
      (has-signal left l)
      (has-signal right r)
      (fd/eq (= (bit-and l r) n)))]
   [(l/fresh [left l right r]
      (or-gate left right wire)
      (has-signal left l)
      (has-signal right r)
      #_(fd/eq (= (bit-or l r) n)))]
   [(l/fresh [input i b]
      (left-shift input b wire)
      (has-signal input i)
      #_(fd/eq (= (bit-shift-left i b) n)))]
   [(l/fresh [input i b]
      (right-shift input b wire)
      (has-signal input i)
      #_(fd/eq (= (bit-shift-right i b) n)))]
   [(l/fresh [input i]
      (not-gate input wire)
      (signal input i)
      (fd/eq (= (bit-not i) n)))]))

(defn signal [wire facts]
  (pldb/with-db facts
    (l/run* [q]
      (l/fresh [a]
        (has-signal wire a)
        (l/== q a)))))

(defn parse-instructions [s]
  (let [ws (str/split s #" ")]
    (cond
      (= 3 (count ws))
      (let [[n _ w] ws] {:gate :SIG :n (Integer/parseInt n) :wire w})
      (= 4 (count ws))
      (let [[g i _ w] ws] {:gate (keyword g) :input i :wire w})
      :else
      (let [[l g r _ w] ws] 
        (if (#{"LSHIFT" "RSHIFT"} g)
          {:gate (keyword g) :input l :b (Integer/parseInt r) :wire w}
          {:gate (keyword g) :left l :right r :wire w})))))

(defn ingest-rule [db {:keys [gate wire n left right input b]}]
  (case gate
    :SIG    (apply pldb/db-fact db signal wire n)
    :AND    (apply pldb/db-fact db and-gate left right wire)
    :OR     (apply pldb/db-fact db or-gate left right wire)
    :LSHIFT (apply pldb/db-fact db left-shift input b wire)
    :RSHIFT (apply pldb/db-fact db right-shift input b wire)
    :NOT    (apply pldb/db-fact db not-gate input wire)))

(defn puzzle1 [wire in]
  (->> in
       str/split-lines
       (map parse-instructions)
       #_(reduce ingest-rule (pldb/db))
       #_(signal wire)))

(comment
  (let [input "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"] (puzzle1 "a" input)))


;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;


(defn parse [input]
  (reduce (fn [m inst]
            (let [[a b c d e] (string/split inst #" ")]
              (cond
                (= b "->")     (assoc m c {:value a})
                (= a "NOT")    (assoc m d {:op bit-not :args [b]})
                (= b "AND")    (assoc m e {:op bit-and :args [a c]})
                (= b "OR")     (assoc m e {:op bit-or :args [a c]})
                (= b "LSHIFT") (assoc m e {:op bit-shift-left :args [a c]})
                (= b "RSHIFT") (assoc m e {:op unsigned-bit-shift-right :args [a c]}))))
          {}
          input))

(def parse-value
  (memoize
   (fn [env value]
     (let [v (read-string value)]
       (if (integer? v)
         v
         (evaluate env value))))))

(defn evaluate [env k]
  (let [{:keys [op args value]} (get env k)]
    (if (nil? op)
      (parse-value env value)
      (apply op (map (partial parse-value env) args)))))

(defn solve [parsed-input]
  (bit-and 0xffff (evaluate parsed-input "a")))

(defn solve-part1 [input]
  (solve (parse input)))

(defn solve-part2 [input]
  (let [input (parse input)]
    (solve (assoc input "b" {:value (str (solve input))}))))

;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;


(ns assembly
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io    :refer [reader]]
            [clojure.string     :as string]))

(defn register-op
  "Update wire entry with function to apply when all arguments are present."
  [[_ _ _ & waiting] arity f]
  (vec (concat [:rv arity f] waiting)))

(declare fwd-arg)
(defn connect
  "Connect the output of `in` to the input of `out`."
  [chans in out]
  (match [(chans in)]
    [([:rv & _] :seq)] (update-in chans [in] conj out)
    [[:tx v]]          (fwd-arg chans v out)
    :else              (assoc chans in [:rv -1 nil out])))

(defn fwd-arg
  "Forward the value `v` to the wire named `out`. This will potentially trigger
  further forwards of values, as dependencies get resolved."
  [chans v out]
  (match [(chans out)]
    [([:rv 1 f & waiting] :seq)]
    (reduce #(connect %1 out %2)
            (assoc chans out [:tx (f v)])
            waiting)

    [([:rv a f & waiting] :seq)]
    (-> chans
        (update-in [out 1] dec)
        (update-in [out 2] partial v))))

(defn populate-args
  "Function to update the entry for `out` with inputs, which can either be wires
  or constant values."
  [out]
  (fn [chans in]
    (cond
      (symbol? in) (connect chans (keyword in) out)
      (number? in) (fwd-arg chans in           out))))

(defn op
  "Introduce an operation that transforms values at `ins` by `f` and puts the
  returned value on `out`."
  [chans arity f out & ins]
  (let [out*  (keyword out)
        ins*  (map read-string ins)
        chans (update-in chans [out*] register-op arity f)]
    (reduce (populate-args out*) chans ins*)))

(defn lsl-16 [s] #(bit-and 0xFFFF (bit-shift-left  % s)))
(defn rsl-16 [s] #(bit-and 0xFFFF (bit-shift-right % s)))
(defn not-16 [x]  (bit-and 0xFFFF (bit-not         x)))

(defn parse-line [chans l]
  (match [(string/split l #" ")]
    [[n            "->" x]] (op chans 1 identity x n)
    [[x "AND"    y "->" z]] (op chans 2 bit-and z x y)
    [[x "OR"     y "->" z]] (op chans 2 bit-or  z x y)
    [[p "LSHIFT" q "->" r]] (op chans 1 (lsl-16 (read-string q)) r p)
    [[p "RSHIFT" q "->" r]] (op chans 1 (rsl-16 (read-string q)) r p)
    [["NOT"      e "->" f]] (op chans 1 not-16 f e)

    :else (println "*** unrecognized ***\n" l)))

(defn parse-file [fname]
  (with-open [fh (reader fname)]
    (reduce parse-line {} (line-seq fh))))

;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;



(ns adventofcode.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "day7.txt"
               io/resource
               io/file
               slurp
               str/split-lines
               doall))

(def part-two? true)

(def wires (atom {}))
(def  get' (memoize #((get @wires %))))
(defn set' [name val] (swap! wires assoc name val))

(defn str->signal [signal-str]
  (condp re-find signal-str
    #"(\w+) RSHIFT (\d+)" :>> (fn [[_ x n]] #(bit-shift-right (get' x) (read-string n)))
    #"(\w+) LSHIFT (\d+)" :>> (fn [[_ x n]] #(bit-shift-left (get' x) (read-string n)))
    #"(\d+) AND (\w+)"    :>> (fn [[_ n x]] #(bit-and (read-string n) (get' x)))
    #"(\w+) AND (\d+)"    :>> (fn [[_ x n]] #(bit-and (read-string n) (get' x)))
    #"(\w+) AND (\w+)"    :>> (fn [[_ x y]] #(bit-and (get' x) (get' y)))
    #"(\d+) OR (\w+)"     :>> (fn [[_ n x]] #(bit-or (read-string n) (get' x)))
    #"(\w+) OR (\d+)"     :>> (fn [[_ x n]] #(bit-or (read-string n) (get' x)))
    #"(\w+) OR (\w+)"     :>> (fn [[_ x y]] #(bit-or (get' x) (get' y)))
    #"NOT (\w+)"          :>> (fn [[_ x]]   #(bit-and 16rFFFF (bit-not (get' x))))
    #"(\d+)"              :>> (fn [[_ n]]   (constantly (read-string n)))
    #"(\w+)"              :>> (fn [[_ x]]   #(get' x))))

(defn parse-str [line]
  (let [[signal-str name] (str/split line #" -> ")
        signal (if (and (= name "b")
                        part-two?)
                 (constantly 956)
                 (str->signal signal-str))]
    (set' name signal)))

(defn solution []
  (for [line input] (parse-str line))
  (get' "a"))