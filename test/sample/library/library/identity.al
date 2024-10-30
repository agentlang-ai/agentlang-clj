(component
 :Library.Identity
 {:clj-import '[(:require [agentlang.lang.string :as str])]})

(defn user-name? [s]
  (str/string-in-range? 2 50 s))

(defn password? [s]
  (or (agentlang.util.hash/crypto-hash? s)
      (str/string-in-range? 3 20 s)))

(entity {:User
         {:UserName    {:type   :String
                        :check  library.identity/user-name?
                        :unique true}
          :Designation {:oneof ["incharge" "general" "intern"]}
          :Password    {:type  :Password
                        :check library.identity/password?}
          :Email       {:type   :Email
                        :guid true}
          :DateCreated :Now}})

(entity {:Member
         {:Name     {:type  :String
                     :check library.identity/user-name?}
          :UserName {:type   :String
                     :check  library.identity/user-name?
                     :unique true}
          :Password {:type  :Password
                     :check library.identity/password?}
          :Email    {:type   :Email
                     :guid true}
          :DOB      :String
          :DateCreated :Now
          ;; Membership Types obtained from here.
          ;; https://www.londonlibrary.co.uk/join/types-of-membership
          :Designation {:oneof ["life" "individual" "family"
                                "remote" "oversees" "supported" "associate" "temporary"]}}})
