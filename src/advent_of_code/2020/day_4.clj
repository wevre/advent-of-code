(ns advent-of-code.2020.day-4
  (:require [clojure.string :as str]
            [clojure.set]))

(defn keys-present? [pp]
  (clojure.set/subset? #{:hgt :pid :byr :eyr :iyr :ecl :hcl} (set (keys pp))))

(defn valid? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and
   (<= 1920 (Integer/parseInt byr) 2002)
   (<= 2010 (Integer/parseInt iyr) 2020)
   (<= 2020 (Integer/parseInt eyr) 2030)
   (when-let [[_ n unit] (re-find #"(\d+)(cm|in)" hgt)]
     (case unit
       "cm" (<= 150 (Integer/parseInt n) 193)
       "in" (<= 59 (Integer/parseInt n) 76)
       false))
   (re-matches #"#[0-9a-f]{6}" hcl)
   (re-matches #"\d{9}" pid)
   (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)))

(defn parse [entries]
  (into {} (map (fn [[k w]] [(keyword k) w]) (partition 2 entries))))

(defn puzzle [in pred]
  (let [passports (->> (str/split in #"\n\n")
                       (map #(re-seq #"[\w#]+" %))
                       (map parse))]
    (count (filter pred passports))))

(comment
  
  (puzzle (slurp "input/2020/4-passports.txt") keys-present?)
  (puzzle (slurp "input/2020/4-passports.txt") (every-pred keys-present? valid?))
  
  (let [input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"]
    [(puzzle input keys-present?)
     (puzzle input (every-pred keys-present? valid?))])
  )