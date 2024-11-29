(ns agentlang.auth.jwt
  (:require [clojure.string :as str]
            [buddy.core.keys.jwk.proto :as buddy-jwk]
            [buddy.sign.jwt :as jwt]
            [clojure.algo.generic.functor :refer [fmap]]
            [agentlang.util.logger :as log]
            [agentlang.datafmt.json :as json])
  (:import [org.jose4j.jwt JwtClaims]
           [org.jose4j.jwt.consumer JwtConsumer JwtConsumerBuilder]))

(defn decode [token]
  (let [^JwtConsumer consumer (-> (JwtConsumerBuilder.)
                                  (.setSkipAllValidators)
                                  (.setDisableRequireSignature)
                                  (.setSkipSignatureVerification)
                                  (.build))
        ^JwtClaims claims (.processToClaims consumer token)]
    (json/decode (.getRawJson claims))))

;; Copied from `https://github.com/sikt-no/clj-jwt/blob/main/src/no/nsd/clj_jwt.clj`
;; because not able to add it as a dependency in `project.clj`.
;; Opened an issue for that: https://github.com/sikt-no/clj-jwt/issues/2
;; Will remove this code once the issue is resolved.
(defn- jwks-edn->keys
  "Transform a vector of json web keys into a map of kid -> key pairs where each key is a map
  of :public-key and optionally :private-keys."
  [json-web-keys]
  (->> json-web-keys
       :keys
       (filter #(= (:kty %) "RSA"))
       (group-by :kid)
       (fmap first)
       (fmap #(assoc {}
                     :public-key (buddy-jwk/jwk->public-key %)
                     :private-key (buddy-jwk/jwk->private-key %)))))

(defn- fetch-keys
  "Fetches the jwks from the supplied jwks-url and converts to java Keys.
  Returns a map keyed on key-id where each value is a RSAPublicKey object"
  [jwks-url]
  (try
    (->> (slurp jwks-url)
         json/decode
         jwks-edn->keys)
    (catch Exception _e
      false)))

;; Atom to hold the public and private keys used for signature validation in memory for
;; caching purposes. The atom holds a clojure map with kid -> key pairs. Each key is a
;; clojure map containing a :public-key and optionally a :private-key.
(defonce keystore
  (atom {}))

(defn- resolve-key
  "Returns java.security.Key given key-fn, jwks-url and :key-type in jwt-header.
  If no key is found refreshes"
  [key-type jwks-url jwt-header]
  (let [key-fn (fn [] (get-in @keystore [jwks-url (:kid jwt-header) key-type]))]
    (if-let [key (key-fn)]
      key
      (do
        (when-let [new-keys (fetch-keys jwks-url)]
          (swap! keystore #(update % jwks-url merge new-keys)))
        (if-let [key (key-fn)]
          key
          (throw
           (ex-info
            (str "Could not locate key corresponding to jwt header's kid: "
                 (:kid jwt-header)
                 " for url: "
                 jwks-url)
            {:type :validation :cause :unknown-key})))))))

(def resolve-public-key
  "Returns java.security.PublicKey given jwks-url and :kid in jwt-header.
  If no key is found refreshes"
  (partial resolve-key :public-key))

(defn remove-bearer [token]
  (if (and token (str/starts-with? (str/lower-case token) "bearer "))
    (subs token (count "Bearer "))
    token))

(defn verify-and-extract
  "Given jwks-url, token, and optionally opts validates and returns the claims
  of the given json web token. Opts are the same as buddy-sign.jwt/unsign."
  ([jwks-url token]
   (verify-and-extract jwks-url token {}))
  ([jwks-url token opts]
   (let [token (remove-bearer token)]
     (try
       (jwt/unsign
        token
        (partial resolve-public-key jwks-url)
        (merge {:alg :rs256} opts))
       (catch Exception e
         (log/warn e))))))
