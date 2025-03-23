(component
 :Agentlang.Kernel.Identity
 {:refer [:Agentlang.Kernel.Lang]
  :clj-import '[(:require [agentlang.lang.internal :as li]
                          [agentlang.util :as u])]})

(entity
 :User
 {:Name {:type :String
         :optional true}
  :Password {:type :Password
             :optional true} ; may use social-login
  :FirstName {:type :String
              :optional true}
  :LastName {:type :String
             :optional true}
  :Email {:type :Email
          li/guid true}
  :UserData {:type :Any :optional true}
  :AppId {:type :UUID
          :default u/uuid-string
          :indexed true}})

(event
 :SignUp
 {:User :User})

(event
 :PostSignUp
 {:SignupRequest :SignUp
  :SignupResult :Any})

(dataflow
 :SignUp
 {:User {} :from :SignUp.User})

(entity
 :UserExtra
 {:User :User
  :OtherDetails :Map})

(entity
 :UserSession
 {:User {:type :String :guid true}
  :LoggedIn :Boolean})

(entity
 :SessionCookie
 {:Id {:type :String :guid true}
  :UserData :Any
  :CreatedTimeMillis :Int64
  :TtlMs {:type :Int64 :default 3600000}})

(event
 :UpdateUser
 {:UserDetails :UserExtra})

(event
 :ForgotPassword
 {:Username :Email})

(event
 :ConfirmForgotPassword
 {:Username :Email
  :ConfirmationCode :String
  :Password :String})

(event
 :ConfirmSignUp
 {:Username :Email
  :ConfirmationCode :String})

(event
 :ChangePassword
 {:AccessToken :String
  :CurrentPassword :String
  :NewPassword :String})

(event
 :RefreshToken
 {:RefreshToken :String})

(event
 :UserLogin 
 {:Username :String
  :Password :Password})

(event
 :OnUserLogin
 {:Username :String})

(event
 :FindUser
 {:Email :Email})

(dataflow
 :FindUser
 {:User
  {:Email? :FindUser.Email}})

(event
 :ResendConfirmationCode
 {:Username :Email})

(dataflow
 [:after :delete :Agentlang.Kernel.Identity/User]
 [:delete :Agentlang.Kernel.Rbac/InstancePrivilegeAssignment {:Assignee :Instance.Email}]
 [:delete :Agentlang.Kernel.Rbac/InstancePrivilegeAssignment :purge]
 [:delete :Agentlang.Kernel.Rbac/OwnershipAssignment {:Assignee :Instance.Email}]
 [:delete :Agentlang.Kernel.Rbac/OwnershipAssignment :purge]
 [:delete :Agentlang.Kernel.Rbac/RoleAssignment {:Assignee :Instance.Email}]
 [:delete :Agentlang.Kernel.Rbac/RoleAssignment :purge])
