(component :Slack.Core)

(dataflow
 :ListUserGroups
 {:SlackWebAPI.Core/usergroups_list
  {:token ""
   :EventContext {:security {:slackAuth {:bearer_token (agentlang.util/getenv "SLACK_API_KEY")}}}}})

(dataflow
 :SendMessage
 {:SlackWebAPI.Core/chat_postMessage
  {:token (agentlang.util/getenv "SLACK_API_KEY")
   :channel (agentlang.util/getenv "SLACK_CHANNEL_ID")
   :text :SendMessage.Text}})

(defn format-news [news]
  (let [docs (get-in news [:response :docs])]
    (reduce
     (fn [s hdline]
       (str s "\n  - " hdline))
     "News feed: "
     (mapv (fn [d] (get-in d [:headline :main])) docs))))

(dataflow
 :PushNews
 {:ArticleSearchAPI.Core/GetArticlesearchJson
  {:q :PushNews.q}
  :as :News}
 [:call (quote (slack.core/format-news :News)) :as :Feed]
 {:SendMessage {:Text :Feed}})

(dataflow
 :Agentlang.Kernel.Lang/AppInit
 {:ArticleSearchAPI.Core/ApiConfig
  {:apikey {:api-key (agentlang.util/getenv "NYT_API_KEY")}}})
