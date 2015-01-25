(ns burnand.utils
  (:require [clj-time.core :as tm]
            [monger.joda-time]
            [clj-time.coerce :as tmc]
            [clj-time.format :as tmf]))

(def date-format (tmf/formatter "d MMMM yyyy"))

(def date-format-short (tmf/formatter "dd-MM-yyyy"))

(def date-format-js (tmf/formatter "dd/MM/yyyy"))

(defn date-string-today [] (tmf/unparse date-format-short (tm/now)))

(defn date-short [date]
   (tmf/unparse (tmf/with-zone date-format-short
                  (tm/time-zone-for-id "Europe/Paris"))
                date))

(defn day-range [date1 date2]
  (take-while #(neg? (compare % date2)) (map #(clj-time.core/plus date1 (clj-time.core/days %)) (range))))

(defn currency [d]
  (format "€ %.2f" d))

(defn month-list []
  (let [now (tm/now)
        custom-formatter (tmf/formatter "MMMM yyyy")]
    (map #(tmf/unparse custom-formatter (tm/plus now (tm/months %))) (range -2 6))))

(defn month-list-numeric []
  (let [now (tm/now)
        custom-formatter (tmf/formatter "MM/yyyy")]
    (map #(tmf/unparse custom-formatter (tm/plus now (tm/months %))) (range -2 6))))

(defn parse-date [d]
  (tmf/parse date-format-short d))

(defn current-year []
  (mod (tm/year (tm/today)) 1000))

(defn today []
  (tm/today-at 00 00))
