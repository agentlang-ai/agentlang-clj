(ns agentlang.store.jdbc-cp
  "Generic connection pooling for JDBC datasources."
  (:require [next.jdbc :as jdbc])
  (:import [javax.sql DataSource] 
           [com.mchange.v2.c3p0 ComboPooledDataSource DataSources]))

(defn ^DataSource as-pooled [^DataSource unpooled stmt-cache-size]
  (DataSources/pooledDataSource unpooled stmt-cache-size))

(defn- setup-datasource [^ComboPooledDataSource ds dbspec]
  (case (:dbtype dbspec)
    :postgres
    (do (.setDriverClass ds "org.postgresql.Driver")
        (.setJdbcUrl ds (str "jdbc:postgresql://" (:host dbspec) "/" (:dbname dbspec)))
        (.setUser ds (:user dbspec))
        (.setPassword ds (:password dbspec))
        ds)
    :sqlite
    (do (.setDriverClass ds "org.sqlite.Driver")
        (.setJdbcUrl ds (str "jdbc:sqlite://" (:dbpath dbspec)))
        ds)
    (throw (Exception. (str "Unsupported database: " (:dbtype ds))))))

(defn init-pool
  "Create a pooled-data-source for the given JDBC driver settings."
  [dbspec]
  ;; May benefit from statement pooling: https://www.mchange.com/projects/c3p0/#configuring_statement_pooling
  ;; Do a benchmark before adding this.
  ;; {:dbtype postgres, :host localhost, :dbname test, :user postgres, :password posterV8}
  (let [^ComboPooledDataSource ds (ComboPooledDataSource.)]
    (setup-datasource ds dbspec)))

(defn open-pooled-datasource [dbspec]
  (let [^ComboPooledDataSource ds (ComboPooledDataSource.)]
    (.setDriverClass ds (:driver-class dbspec))
    (.setJdbcUrl ds (:jdbc-url dbspec))
    (.setUser ds (:username dbspec))
    (.setPassword ds (:password dbspec))
    ds))

(defn open-non-pooled-datasource [dbspec]
  (jdbc/get-datasource dbspec))

(defn close-pooled-datasource [^ComboPooledDataSource ds]
  (.close ds))
