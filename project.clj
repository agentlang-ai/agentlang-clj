(defproject com.github.agentlang-ai/agentlang "0.6.1-alpha10"
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/clojurescript "1.11.132"
                  :exclusions [com.google.code.findbugs/jsr305]]
                 [org.clojure/core.memoize "1.0.257"]
                 [org.clojure/tools.cli "1.0.206"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/tools.logging "1.2.4"]
                 [org.slf4j/slf4j-api "2.0.12"]
                 [org.clojure/core.async "1.6.681"]
                 [clojure.java-time "1.4.2"]
                 [org.clojure/test.check "1.1.1"]
                 [ch.qos.logback/logback-classic "1.5.3"]
                 [environ "1.2.0"]
                 [commons-io/commons-io "2.11.0"]
                 [org.apache.commons/commons-exec "1.3"]
                 [cheshire "5.10.1"]
                 [com.github.seancorfield/next.jdbc "1.3.883"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [selmer "1.12.58"]
                 [com.h2database/h2 "1.4.200"]
                 [redis.clients/jedis "5.1.2"]
                 [org.mindrot/jbcrypt "0.4"]
                 [honeysql "1.0.461"]
                 [compojure "1.7.1"]
                 [http-kit "2.7.0"]
                 [cljs-http "0.1.48"]
                 [ring-cors "0.1.13"]
                 [keycloak-clojure "1.31.2"]
                 [net.cgrand/macrovich "0.2.1"]
                 [cljsjs/alasql "0.6.5-0"]
                 [org.postgresql/postgresql "42.3.1"]
                 [com.pgvector/pgvector "0.1.4"]
                 [com.widdindustries/cljc.java-time "0.1.21"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [com.cognitect/transit-cljs "0.8.269"]
                 [buddy/buddy-auth "3.0.323"]
                 [org.bitbucket.b_c/jose4j "0.7.12"]
                 [reagent "1.1.0"]
                 [cljsjs/react "17.0.2-0"]
                 [tick "0.5.0-RC1"]
                 [spec-provider "0.4.14"]
                 [amazonica "0.3.162"]
                 [buddy/buddy-core "1.6.0"]
                 [buddy/buddy-sign "3.1.0"]
                 [org.clojure/algo.generic "0.1.3"]
                 [metosin/ring-swagger "0.26.2"]
                 [cheshire "5.11.0"]
                 [metosin/malli "0.16.4"]
                 [com.github.scribejava/scribejava-core "8.3.3"]
                 [com.github.scribejava/scribejava-apis "8.3.3"]
                 [org.apache.kafka/kafka-clients "3.6.1"]
                 [com.walmartlabs/lacinia "1.2.2"]
                 [com.clickhouse/clickhouse-jdbc "0.6.0"]
                 [com.github.fractl-io/fractl-config-secrets-reader "0.1.0"]
                 [nrepl "1.1.1"]
                 [camel-snake-kebab "0.4.3"]
                 [stringer "0.4.1"]
                 [nrepl/drawbridge "0.2.1"]]

  :license {:name "Apache2"}

  :java-source-paths ["src/java"]

  :main agentlang.core
  :aot :all
  ;;:omit-source true

  :jar-exclusions [#"(?:^|/).agentlang/" #"(?:^|/).db/" #"(?:^|/).json/"]

  :uberjar-exclusions [#"(?:^|/).agentlang/" #"(?:^|/).db/" #"(?:^|/).json/"]

  :plugins [[lein-cljsbuild "1.1.8" :exclusions [[org.clojure/clojure]]]
            [lein-environ "1.2.0"]
            [s3-wagon-private "1.3.4"]
            [lein-doo "0.1.10"]
            [reifyhealth/lein-git-down "0.4.0"]
            [lein-ancient "1.0.0-RC3"]
            [cider/cider-nrepl "0.37.1"]
            [refactor-nrepl "3.10.0"]
            [lein-classpath-jar "0.1.0"]]

  :middleware [lein-git-down.plugin/inject-properties]

  :git-down {de.active-group/active-logger {:coordinates kitrerp/active-logger}}

  :repositories [["public-github" {:url "git://github.com" :protocol :https}]]

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :sign-releases false}]]

  :repl {:dependencies [[nrepl "1.1.1"]]}

  :resource-paths ["resources" "target/classes"]

  :pom-addition [:distributionManagement
                 [:repository
                  ["id" "github"]
                  ["name" "GitHub agentlang-ai Apache Maven Packages"]
                  ["url" "https://maven.pkg.github.com/agentlang-ai/agentlang"]]]

  :profiles {:dev {:dependencies [[com.bhauman/rebel-readline-cljs "0.1.4" :exclusions [args4j]]
                                  [com.bhauman/figwheel-main "0.2.15"
                                   :exclusions [args4j
                                                com.google.code.findbugs/jsr305
                                                org.clojure/java.classpath]]]
                   ;; setup target as a resource path
                   :resource-paths ["target" "resources" "node_modules"]

                   ;; set up an alias to invoke your figwheel build
                   :aliases  {"figwheel"  ["trampoline" "run" "-m" "figwheel.main"]
                              "fig:ui" ["trampoline" "run" "-m" "figwheel.main" "-co" "test/ci/ui.cljs.edn" "-r"]
                              "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
                              "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
                              "fig:test"  ["run" "-m" "figwheel.main" "-co" "test/ci/test.cljs.edn" "-m" "agentlang.test-runner"]
                              "fig:rtest"  ["run" "-m" "figwheel.main" "-co" "test/ci/test.cljs.edn" "-m" "agentlang.reagent-test-runner"]
                              "fig:ci"  ["run" "-m" "figwheel.main" "-co" "test/ci/ci.cljs.edn" "-m" "agentlang.test-runner"]
                              "fig:rci"  ["run" "-m" "figwheel.main" "-co" "test/ci/ci.cljs.edn" "-m" "agentlang.reagent-test-runner"]}
                   :clean-targets  ^{:protect false} ["target" "out"]}
             :with-model {:javac-options ["-target" "11" "-source" "11" "-Xlint:-options"]
                          :resource-paths ["app"]}})
