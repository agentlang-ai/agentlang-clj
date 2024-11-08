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
    (catch
     #?(:clj Exception :cljs :default) ex
      (log/error ex)
      false)))

(defn try-parse-date-time [formatter s]
  (valid-format? ldt/parse formatter s))

(defn- try-parse-date [formatter s]
  (valid-format? ld/parse formatter s))

(defn- try-parse-time [formatter s]
  (valid-format? lt/parse formatter s))

(def date-time-formatters
  (concat
   [format/iso-local-date-time]  ; 2011-12-03T10:15:30
   (mapv
    format/of-pattern
    ["yyyy-MM-dd HH:mm:ss"       ; 2021-01-08 04:05:06
     "yyyy-MM-dd HH:mm"          ; 2021-01-08 04:05
     "yyyy-MM-dd HH:mm:ss.SSS"   ; 2021-01-08 04:05:06.789
     "yyyyMMddHHmmss"            ; 20210108040506
     "yyyy-MM-dd HH:mm:ss z"]))) ; 2021-01-08 04:05:06 PST or America/New_York

(def ^:private date-formatters
  (map
   format/of-pattern
   ["MMMM d, yyyy" ; January 8, 2021
    "yyyy-MMM-dd"  ; 2021-Jan-08
    "yyyy-MM-dd"   ; 2021-01-30
    "MMM-dd-yyyy"  ; Jan-08-2021
    "dd-MMM-yyyy"  ; 08-Jan-2021
    "yyyyMMdd"]))  ; 20210108

(def ^:private time-formatters
  (map
   format/of-pattern
   ["HH:mm:ss.SSS"  ; 04:05:06.789
    "HH:mm:ss"      ; 04:05:06
    "HH:mm"         ; 04:05
    "HHmmss"        ; 040506
    ;;; 12-hour format time is manually parsed because
    ;;; Java11 and Java14 behave differently for the
    ;;; following pattern.
    ;; "hh:mm a"    ; 04:05 pm, hour <= 12
    "HH:mm:ss z"])) ; 04:05:06 PST or 04:05:06 America/New_York

(defn find-format [try-parse-fn formatters s]
  (some #(try-parse-fn % s) formatters))

#?(:cljs
   (defn- js-parse-date [s]
     ;; TODO: Implement proper date-time parsing in cljs
     (if t/date-time
       (t/date-time s)
       s)))

(defn parse-date-time [s]
  #?(:clj
     (find-format try-parse-date-time date-time-formatters s)
     :cljs
     (js-parse-date s)))

(defn parse-date [s]
  #?(:clj
     (find-format try-parse-date date-formatters s)
     :cljs
     (js-parse-date s)))

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

(defn parse-time [s]
  #?(:clj
     (or (find-format try-parse-time time-formatters s)
         (parse-12hr-time s))
     :cljs
     ;; TODO: Implement proper date-time parsing in cljs
     (if t/time
       (t/time s)
       s)))

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
