(component
  :GithubIssueAutolabel.Resolver
  {:clj-import
   (quote [(:require [clojure.string :as s]
                     [agentlang.component :as fc]
                     [agentlang.evaluator :as fe]
                     [cheshire.core      :as json]
                     [org.httpkit.client :as http])])})


;; ----- Github helpers -----


(defn teeprn
  [x]
  (print "\n[teeprn] ")
  (prn x)
  x)

(defn github-headers [token]
  {"Accept"               "application/vnd.github+json"
   "Authorization"        (str "Bearer " token)
   "X-GitHub-Api-Version" "2022-11-28"})

(defn github-get-issues [token owner repo]
  (let [api-url (format "https://api.github.com/repos/%s/%s/issues" owner repo)
        headers (github-headers token)
        response (http/get api-url {:headers headers})]
    (->> (:body @response)
         json/parse-string
         (mapv (fn [m] (select-keys m ["url" "title" "body" "labels" "state"]))))))

(defn github-update-issue-labels [token issue-url labels]
  (let [headers  (github-headers token)
        bodystr  (json/generate-string {:labels labels})
        response (http/patch issue-url {:headers headers
                                        :body bodystr})]
    @response
    nil))


;; ----- Resolver -----


(entity
  :GithubIssueAutolabel.Resolver/Config
  {:meta {:inherits :Agentlang.Kernel.Lang/Config}
   :Token :String
   :Owner :String
   :Repo  :String
  })

(entity
  :GithubIssueAutolabel.Resolver/Issue
  {:Url    :String
   :Title  :String
   :Body   :String
   :Labels {:listof :Map}
   :State  :String})

(defn issue-query [[entity-name {clause :where} :as param]] (println "Entered [issue-query] with" param)
  ;; parameter format is [entity-name {:from entity-name :where clause}]
  ;; entity-name will be in the format [:ComponentName :Entity]
  ;; `clause` will be `[logical-opr [comparison-opr attribute-name attribute-value] ...]`
  ;; where `logical-opr` could be one of `:and` , `:or` and this is optional
  ;; `comparison-opr` is one of `:=`, `:<`, `:<=`, `:>` `:>=`, `:like`, `:between`
  ;; this is generated from dataflow query-patterns, it's the resolvers job to translate
  ;; this intermediate representation to a query legal for the backend
  (let [[component-name entity-name] entity-name
        config (fe/fetch-model-config-instance :GithubIssueAutolabel)
        _ (when-not (map? config)
            (throw (ex-info "[GithubIssueAutolabel.Resolver/issue-query] Config is not a map" {:config config})))
        token  (:Token config)
        owner  (:Owner config)
        repo   (:Repo  config)]
    (->> (github-get-issues token owner repo)
         teeprn
         (mapv (fn [m]
                 {:Url    (get m "url")
                  :Title  (get m "title")
                  :Body   (get m "body")
                  :Labels (vec (get m "labels"))
                  :State  (get m "state")}))
         (filterv (fn [m] (empty? (:Labels m)))) ; keep only unlabeled issues
         (mapv (partial fc/make-instance [:GithubIssueAutolabel.Resolver :Issue]))
         teeprn
         )))

(entity
  :GithubIssuesDemo.Resolver/IssueTriage
  {:Url      :String
   :Severity {:type :String :optional true}
   :Priority {:type :String :optional true}})

(defn issue-create [instance] (println "Entered [issue-create] with" instance)
  (let [config (fe/fetch-model-config-instance :GithubIssueAutolabel)
        token (:Token config)
        issue-url (:Url instance)
        labels (->> [(:Severity instance) (:Priority instance)]
                    (filterv some?))]
    (when (seq labels)
      (github-update-issue-labels token issue-url labels))))

(resolver
  :github
  {:require {}
   :paths [:GithubIssueAutolabel.Resolver/Issue :GithubIssueAutolabel.Resolver/IssueTriage]
   :with-methods {:query issue-query
                  :create issue-create}})

