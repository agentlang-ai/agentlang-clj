(ns
 agentlang.kernel.identity
 (:require
  [agentlang.lang.internal :as li]
  [agentlang.util :as u]
  [agentlang.kernel.lang
   :refer
   [Agentlang_Kernel_Lang___COMPONENT_ID__]]
  [agentlang.lang
   :refer
   [dataflow
    entity
    view
    pattern
    attribute
    rule
    relationship
    component
    resolver
    event
    inference
    record]]))
(component
 :Agentlang.Kernel.Identity
 {:refer [:Agentlang.Kernel.Lang],
  :clj-import
  '[(:require
     [agentlang.lang.internal :as li]
     [agentlang.util :as u])]})
(entity
 :Agentlang.Kernel.Identity/User
 {:Name {:type :String, :optional true},
  :Password {:type :Password, :optional true},
  :FirstName {:type :String, :optional true},
  :LastName {:type :String, :optional true},
  :Email {:type :Email, li/path-identity true},
  :UserData {:type :Any, :optional true},
  :AppId {:type :UUID, :default u/uuid-string, :indexed true}})
(event
 :Agentlang.Kernel.Identity/SignUp
 {:User :Agentlang.Kernel.Identity/User})
(event
 :Agentlang.Kernel.Identity/PostSignUp
 {:SignupRequest :Agentlang.Kernel.Identity/SignUp, :SignupResult :Any})
(dataflow
 :Agentlang.Kernel.Identity/SignUp
 {:Agentlang.Kernel.Identity/User {},
  :from :Agentlang.Kernel.Identity/SignUp.User})
(entity
 :Agentlang.Kernel.Identity/UserExtra
 {:User :Agentlang.Kernel.Identity/User, :OtherDetails :Map})
(entity
 :Agentlang.Kernel.Identity/UserSession
 {:User {:type :String, :id true}, :LoggedIn :Boolean})
(entity
 :Agentlang.Kernel.Identity/SessionCookie
 {:Id {:type :String, :id true},
  :UserData :Any,
  :CreatedTimeMillis :Int64})
(event
 :Agentlang.Kernel.Identity/UpdateUser
 {:UserDetails :Agentlang.Kernel.Identity/UserExtra})
(event :Agentlang.Kernel.Identity/ForgotPassword {:Username :Email})
(event
 :Agentlang.Kernel.Identity/ConfirmForgotPassword
 {:Username :Email, :ConfirmationCode :String, :Password :String})
(event
 :Agentlang.Kernel.Identity/ConfirmSignUp
 {:Username :Email, :ConfirmationCode :String})
(event
 :Agentlang.Kernel.Identity/ChangePassword
 {:AccessToken :String, :CurrentPassword :String, :NewPassword :String})
(event :Agentlang.Kernel.Identity/RefreshToken {:RefreshToken :String})
(event
 :Agentlang.Kernel.Identity/UserLogin
 {:Username :String, :Password :Password})
(event :Agentlang.Kernel.Identity/OnUserLogin {:Username :String})
(event :Agentlang.Kernel.Identity/FindUser {:Email :Email})
(dataflow
 :Agentlang.Kernel.Identity/FindUser
 #:Agentlang.Kernel.Identity{:User
                             {:Email?
                              :Agentlang.Kernel.Identity/FindUser.Email}})
(event
 :Agentlang.Kernel.Identity/ResendConfirmationCode
 {:Username :Email})
(dataflow
 [:after :delete :Agentlang.Kernel.Identity/User]
 [:delete
  :Agentlang.Kernel.Rbac/InstancePrivilegeAssignment
  {:Assignee :Instance.Email}]
 [:delete :Agentlang.Kernel.Rbac/InstancePrivilegeAssignment :purge]
 [:delete
  :Agentlang.Kernel.Rbac/OwnershipAssignment
  {:Assignee :Instance.Email}]
 [:delete :Agentlang.Kernel.Rbac/OwnershipAssignment :purge]
 [:delete
  :Agentlang.Kernel.Rbac/RoleAssignment
  {:Assignee :Instance.Email}]
 [:delete :Agentlang.Kernel.Rbac/RoleAssignment :purge])
(def
 Agentlang_Kernel_Identity___COMPONENT_ID__
 "9a88e4c1-6807-4027-a892-41e699ea9c5d")
