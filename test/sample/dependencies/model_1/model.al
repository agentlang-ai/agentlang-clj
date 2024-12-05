{:name :Model1
 :agentlang-version "current"
 :components [:Model1.C1]
 :dependencies [[:fs "./model_2"]]
 :model-paths ["./test/sample/dependencies"]
 :config {:service {:port 8080}
          :store {:dbname "./app/db"}}}
