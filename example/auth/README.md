# auth

Session-cookie authentication sample.

## Running

Run agentlang:

    agent -c config.edn example/auth/order.al

where `config.edn` should have the following values:

```clojure
{:service {:port 8000}
 :store {:type :postgres
         :host #$ POSTGRES_HOST
         :dbname #$ POSTGRES_DB
         :username #$ POSTGRES_USER
         :password #$ POSTGRES_PASSWORD}
 :logging {:syslog {}}
 :authentication {:service :okta
                  :superuser-email "superuser@acme.com"
                  :domain "dev-04676848.okta.com"
                  :auth-server "default"
                  :client-id "okta-app-client-id" ; replace this
                  :client-secret "okta-app-client-secret" ; replace this
                  :scope "openid offline_access"
                  :introspect true
                  :authorize-redirect-url "http://localhost:3000/auth/callback"
                  :client-url "http://localhost:3000/order"
                  :cache {:host #$ REDIS_HOST
                          :port #$ REDIS_PORT}}}
```
