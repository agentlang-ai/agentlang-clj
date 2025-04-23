(component :Slack.Core)

(def slack-config
  (agentlang.connections.client/connection-parameter
   (agentlang.connections.client/open-connection :SlackWebAPI.Core/Connection)))

(dataflow
 :SendMessage
 {:SlackWebAPI.Core/chat_postMessage
  {:token (:token slack-config)
   :channel (:channel slack-config)
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
