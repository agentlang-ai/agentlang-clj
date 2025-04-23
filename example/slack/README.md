These integration-configs must exist:

```json
POST api/IntegrationManager.Core/Integration/NYT/ConnectionConfigGroup/ConnectionConfig

{"IntegrationManager.Core/ConnectionConfig": 
  {
        "UserData": {
          "name": "ArticleSearchAPI.Core/Connection",
          "type": "ArticleSearchAPI.Core/ApiConfig",
          "title": "Configure NYT Credentials",
          "description": "provide nyt creds",
          "ConnectionTypeName": "ArticleSearchAPI.Core/Connection"
        },
        "Parameter": 
          {"apikey": {"api-key": "<api-key>"}}
        ,
        "Type": "custom", 
        "Name": "nyt01"
  }
}
```

```json
POST api/IntegrationManager.Core/Integration/Slack/ConnectionConfigGroup/ConnectionConfig

{"IntegrationManager.Core/ConnectionConfig": 
  {
        "UserData": {
          "name": "SlackWebAPI.Core/Connection",
          "type": "SlackWebAPI.Core/ApiConfig",
          "title": "Configure Slack Credentials",
          "description": "provide slack creds",
          "ConnectionTypeName": "SlackWebAPI.Core/Connection"
        },
        "Parameter": 
          {"channel": "<channel-id>", "token": "<api-token>"}
        ,
        "Type": "custom", 
        "Name": "slack01"
  }
}
```
