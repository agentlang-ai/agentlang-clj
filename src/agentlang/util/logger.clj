(ns agentlang.util.logger
  (:require [clojure.tools.logging :as logger])
  (:import [java.io File]
           [org.slf4j LoggerFactory]
           [ch.qos.logback.classic Level]
           [ch.qos.logback.classic Logger]
           [ch.qos.logback.classic LoggerContext] 
           [ch.qos.logback.classic.net SyslogAppender]
           [ch.qos.logback.classic.joran JoranConfigurator]))

;; flag applies only to low-priority log-modes (debug and info)
(def logging-enabled (atom true))
(def dev-logging-enabled (atom false))

(defn disable-logging! [] (reset! logging-enabled false))
(defn enable-logging! [] (reset! logging-enabled true))

(defn enable-dev-logging! [] (reset! dev-logging-enabled true))

(defn logging-enabled? [] @logging-enabled)
(defn dev-logging-enabled? [] @dev-logging-enabled)

(def log-capture! logger/log-capture!)


(defmacro error [msg] `(logger/error ~msg))

(defmacro debug [msg]
  `(when (logging-enabled?)
     (logger/debug ~msg)))

(defmacro dev-debug [msg]
  `(when (dev-logging-enabled?)
     (debug ~msg)))

(defmacro info [msg]
  `(when (logging-enabled?)
     (logger/info ~msg)))

(defmacro warn [msg] `(logger/warn ~msg))

(defmacro exception [ex]
  `(do (error (.getMessage ~ex))
       (let [^java.io.StringWriter sw# (java.io.StringWriter.)
             ^java.io.PrintWriter pw# (java.io.PrintWriter. sw#)]
         (.printStackTrace ~ex pw#)
         (.close pw#)
         (debug (.toString sw#)))))

(defn- as-level [k]
  (case k
    :debug Level/DEBUG
    :warn Level/WARN
    :info Level/INFO
    :error Level/ERROR
    :trace Level/TRACE
    Level/ALL))

(defn create-syslogger
  ([logger-name config]
   (let [^LoggerContext lc (LoggerFactory/getILoggerFactory)
         ^SyslogAppender appender (SyslogAppender.)]
     (.setSyslogHost appender (or (:syslog-host config) "localhost"))
     (.setPort appender (or (:port config) 514))
     (.setFacility appender (or (:facility config) "SYSLOG"))
     (.setContext appender lc)
     (.start appender)
     (let [^Logger logger (LoggerFactory/getLogger logger-name)]
       (.addAppender logger appender)
       (.setLevel logger (as-level (or (:level config) :debug)))
       (.setAdditive logger (or (:is-additive config) false))
       logger)))
  ([config] (create-syslogger "ROOT" config))
  ([] (create-syslogger nil)))

(defn apply-custom-logback-config [config-path]
  (let [^LoggerContext context (LoggerFactory/getILoggerFactory)
        configurator (JoranConfigurator.)]
    (.reset context)
    (.setContext configurator context)
    (.doConfigure configurator (File. config-path))
    (println (str "Applied logback config from: " config-path))))
