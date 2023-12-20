(ns advent-of-code.2023.day-20-pulses
  (:require [clojure.string :as str]
            [advent-of-code.common :as common]
            [advent-of-code.common.remainders :as remainders]))

(defn parse-line [line]
  (let [[_ t n d's] (re-matches #"(\%|\&)?([a-z]+) -> (.*)" line)
        d's (str/split d's #", ")
        t ({"&" :conjunction, "%" :flip-flop} t :broadcast)]
    [n (case t
         :conjunction {:type t :dest d's}
         :flip-flop {:type t :?on false :dest d's}
         :broadcast {:type t :dest d's})]))

(defn parse [input]
  (reduce (fn [m [nxt info]]
            (reduce (fn [m d] (update-in m [d :inp] (fnil conj {}) [nxt :lo]))
                    (update m nxt merge info)
                    (:dest info)))
          {}
          (map parse-line (str/split-lines input))))

(def ?hi #{:hi})
(def ?lo #{:lo})

(defn push-button [{:keys [config pulses]}]
  (loop [queue (common/queue ["button" "broadcaster" :lo]) config config pulses pulses]
    (let [[[frm nxt pp :as step] queue] ((juxt peek pop) queue)]
      (if-not step
        {:config config :pulses pulses}
        (let [pulses (update pulses pp inc)
              {:keys [type dest ?on inp]} (config nxt)]
          (case type
            :broadcast (let [queue (into queue (map (fn [d] [nxt d pp]) dest))]
                         (recur queue config pulses))
            :flip-flop (let [queue (cond-> queue
                                     (?lo pp) (into (map (fn [d] [nxt d (if ?on :lo :hi)]) dest)))
                             config (cond-> config
                                      (?lo pp) (update-in [nxt :?on] not))]
                         (recur queue config pulses))
            :conjunction (let [inp (assoc inp frm pp)
                               config (assoc-in config [nxt :inp] inp)
                               ?all-hi (every? ?hi (vals inp))
                               queue (into queue (map (fn [d] [nxt d (if ?all-hi :lo :hi)]) dest))]
                           (recur queue config pulses))
            (recur queue config pulses)))))))

(defn find-loop [iters module]
  (->> iters
       (map-indexed (fn [i state] [i (get-in state [:config module :?on])]))
       (partition 2 1)
       (some (fn [[a b]] (when (= (second a) (second b)) (first b))))))

(comment
  (def config (parse (slurp "input/2023/20-sample-1.txt")))
  (def config (parse (slurp "input/2023/20-sample-2.txt")))
  (def config (parse (slurp "input/2023/20-pulses.txt")))

  (get config "dr")

  ;; year 2023 day 20 puzzle 1
  (->> (iterate push-button {:config config :pulses {:hi 0 :lo 0}})
       (drop 1000)
       first
       :pulses
       vals
       (reduce *))

  ;; year 2023 day 20 puzzle 2
  (let [iters (iterate push-button {:config config :pulses {:hi 0 :lo 0}})
        ;; Grab the flip-flips that are directly connected to broadcaster. Each
        ;; of those is a branch, and when the other end of the branch finally
        ;; sends a :lo signal to "rx", this flip flop also gets reset. So the
        ;; history of this flip-flop will be alternating on and off, but when
        ;; the one magic pulse goes through, it will make the history show two
        ;; off's in a row. We can use that to find the cycle, and then find the
        ;; LCM of all the branch cycles. This is basically an implementation of
        ;; a MOD counter.
        ff's (:dest (config "broadcaster"))]
    (->> (map #(find-loop iters %) ff's)
         (apply remainders/lcm)))
  ;; => 247702167614647
  )
