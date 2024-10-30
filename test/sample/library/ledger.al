(component
 :Library.Ledger
 {:clj-import '[(:use [agentlang.lang.string]
                      [agentlang.lang.b64])
                (:require [agentlang.component :as cn])]
  :refer [:Library.Identity :Library.Catalog]})

(entity {:CheckoutLog
         {:Member       {:ref     :Library.Identity/Member.Email
                         :indexed true}
          :Designation  :String
          :Book         {:ref     :Library.Catalog/Book.ISBN
                         :indexed true}
          :IsCheckedout {:type    :Boolean
                         :default true}
          :CheckoutDate :Now
          :ReturnDate   {:type     :DateTime
                         :optional true}
          :meta         {:unique [:Member :Book]}}})

(entity {:Authentication
         {:Owner         :Any
          :Issued        {:type :DateTime :optional true}
          :ExpirySeconds {:type :Int :default 300}}})

(dataflow :MemberLogin
          {:Library.Identity/Member {:UserName? :MemberLogin.UserName}}
          [:match :Library.Identity/Member.Password
           :MemberLogin.Password  {:Authentication {:Owner :Library.Identity/Member}}])

(dataflow :UserLogin
          {:Library.Identity/User {:UserName? :Library.Ledger/UserLogin.UserName}}
          [:match :User.Password
           :UserLogin.Password  {:Authentication {:Owner :Library.Identity/User}}])

(event {:CheckoutBook
        {:Member   :Email
         :Designation :String
         :Book     :String
         :Backend  :String
         :Receiver :String
         :Subject  :String
         :Text     :String
         :To       :String}})

(dataflow :CheckoutBook
          {:CheckoutLog {:Member       :CheckoutBook.Member
                         :Designation  :CheckoutBook.Designation
                         :Book         :CheckoutBook.Book
                         :IsCheckedout true}}
          #_{:Email/Push {:Backend  :Test.Sample.Library.Ledger/CheckoutBook.Backend
                          :Receiver :Test.Sample.Library.Ledger/CheckoutBook.Receiver
                          :Subject  :Test.Sample.Library.Ledger/CheckoutBook.Subject
                          :Text     :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          #_{:Sms/Push {:To   :Test.Sample.Library.Ledger/CheckoutBook.To
                        :Body :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          {:CheckoutLog {:Member? :CheckoutBook.Member
                         :Book?   :CheckoutBook.Book}})

(event {:CheckinBook
        {:Member   :Email
         :Designation :String
         :Book     :String
         :Backend  :String
         :Receiver :String
         :Subject  :String
         :Text     :String
         :To       :String}})

(dataflow :CheckinBook
          {:CheckoutLog {:Member       :CheckinBook.Member
                         :Designation   :CheckinBook.Designation
                         :Book         :CheckinBook.Book
                         :IsCheckedout false}}
          #_{:Email/Push {:Backend  :Test.Sample.Library.Ledger/CheckoutBook.Backend
                        :Receiver :Test.Sample.Library.Ledger/CheckoutBook.Receiver
                        :Subject  :Test.Sample.Library.Ledger/CheckoutBook.Subject
                        :Text     :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          #_{:Sms/Push {:To   :Test.Sample.Library.Ledger/CheckoutBook.To
                      :Body :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          {:CheckoutLog {:Member? :CheckinBook.Member
                         :Book?   :CheckinBook.Book}})

(event {:CheckedoutBooks
        {:Member :UUID}})

(dataflow :CheckedoutBooks
          {:CheckoutLog {:Member? :CheckedoutBooks.Member}})

(event {:CheckedoutBy
        {:Book :UUID}})

(dataflow :CheckedoutBy
          {:CheckoutLog {:Book? :CheckedoutBy.Book}}
          [:match :CheckoutLog.IsCheckedout
           true :CheckoutLog])

(event {:AllCheckouts {}})

(dataflow :AllCheckouts
          :CheckoutLog?)

(dataflow :ServicePolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Identity/Upsert_Member"]
            :Spec      [:q#
                        [:when
                         [:in ["life" "individual" "family"
                               "remote" "oversees" "supported" "associate"]
                          :EventContext.Auth.Owner.Designation]]]}})

(dataflow :UserCreationPolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Identity/Upsert_User"]
            :Spec      [:q#
                        [:when
                         [:= "incharge" :EventContext.Auth.Owner.Designation]]]}})

(dataflow :CheckoutPolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Ledger/CheckoutBook"]
            :Spec      [:q#
                        [:when
                         [:in ["life" "individual" "family"
                               "remote" "oversees" "supported" "associate"]
                          :EventContext.Auth.Owner.Designation]]]}})

(dataflow :CheckinPolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Ledger/CheckinBook"]
            :Spec      [:q#
                        [:when
                         [:in ["life" "individual" "family"
                               "remote" "oversees" "supported" "associate"]
                          :EventContext.Auth.Owner.Designation]]]}})

(dataflow :RBACPolicyLogging
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "Logging"
            :Resource  ["Test.Sample.Library.Ledger/CheckoutBook"
                        "Test.Sample.Library.Ledger/CheckinBook"
                        "Test.Sample.Library.Identity/Upsert_Member"
                        "Test.Sample.Library.Identity/Upsert_User"]
            :Spec      [:q# {:Disable        :INFO
                             :PagerThreshold {:WARN  {:count            5
                                                      :duration-minutes 10}
                                              :ERROR {:count            3
                                                      :duration-minutes 5}}}]}})
