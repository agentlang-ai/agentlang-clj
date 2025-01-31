(ns agentlang.lang.datetime
  (:require [clojure.string :as str]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [tick.core :as t]
            [tick.locale-en-us]
            [cljc.java-time.local-date :as ld]
            [cljc.java-time.local-time :as lt]
            [cljc.java-time.local-date-time :as ldt]
            [cljc.java-time.format.date-time-formatter :as format]
            [cljc.java-time.temporal.chrono-unit :as cu]
            [cljc.java-time.zone-id :as zone-id]))

(defn- valid-format? [parser formatter s]
  (try
    (parser s formatter)
    (catch #?(:clj Exception :cljs :default) _
      false)))

(defn try-parse [parse formatter s]
  (valid-format? parse formatter s))

(def try-parse-date-time (partial try-parse ldt/parse))
(def try-parse-date (partial try-parse ld/parse))
(def try-parse-time (partial try-parse lt/parse))

(def default-date-time-format format/iso-local-date-time) ; 2011-12-03T10:15:30
(def default-date-format (format/of-pattern "yyyy-MM-dd"))   ; 2021-01-30
(def default-time-format (format/of-pattern "HH:mm:ss.SSS")) ; 04:05:06.789

#?(:cljs
   (defn- js-parse-date [s]
     ;; TODO: Implement proper date-time parsing in cljs
     (if t/date-time
       (t/date-time s)
       s)))

(defn date-time-parser [formatter]
  #?(:clj
     (let [fmt (format/of-pattern formatter)]
       (fn [s]
         (try-parse-date-time fmt s)))
     :cljs js-parse-date))

(defn parse-default-date-time [s]
  #?(:clj
     (try-parse-date-time default-date-time-format s)
     :cljs
     (js-parse-date s)))

(defn date-parser [formatter]
  #?(:clj
     (let [fmt (format/of-pattern formatter)]
       (fn [s]
         (try-parse-date fmt s)))
     :cljs js-parse-date))

(defn parse-default-date [s]
  #?(:clj
     (try-parse-date default-date-format s)
     :cljs
     (try-parse-date s)))

(defn- am-pm? [s]
  (let [s (str/lower-case s)]
    (or (= s "am") (= s "pm"))))

(defn- parse-12hr-time [s]
  (let [n (count s)]
    (when (and (= n 8)
               (= \: (nth s 2))
               (am-pm? (subs s 6)))
      (let [h (u/parse-string (subs s 0 2))
            m (u/parse-string (subs s 3 5))]
        (and (number? h) (number? m)
             (<= 0 h 12) (<= 0 m 59))))))

(defn time-parser [formatter]
  #?(:clj
     (let [fmt (format/of-pattern formatter)]
       (fn [s]
         (try-parse-time fmt s)))
     :cljs
     ;; TODO: Implement proper date-time parsing in cljs
     #(if t/time
        (t/time %)
        %)))

(defn parse-default-time [s]
  #?(:clj
     (try-parse-time default-time-format s)
     :cljs
     (parse-time s)))

(defn as-string
  ([dt pat]
   (format/format
    (if pat
      (format/of-pattern pat)
      format/iso-local-date-time)
    dt))
  ([dt] (as-string dt nil)))

(defn as-format
  [dt pat]
  (format/format
   (if pat
     (format/of-pattern pat)
     format/iso-local-date-time)
   (ldt/parse dt)))

(defn now []
  (as-string (ldt/now)))

(defn now-utc []
  (as-string (ldt/now (zone-id/of "UTC"))))

#?(:clj (def now-raw ldt/now)
   :cljs (defn now-raw []
           (ldt/now)))

(defn difference-in-seconds [dt1 dt2]
  (cu/between cu/seconds dt1 dt2))

(defn current-time-millis []
  #?(:clj
     (System/currentTimeMillis)
     :cljs
     (. (js/Date.) (getTime))))

(defn unix-timestamp []
  (quot (current-time-millis) 1000))
