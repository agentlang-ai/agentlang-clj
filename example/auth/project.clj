(defproject com.github.agentlang-io/auth "0.1.0"
  :description "Session-cookie authentication sample."
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [hiccup "2.0.0-RC3"]
                 [http-kit "2.5.3"]
                 [cheshire "5.12.0"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler auth.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]]}})
