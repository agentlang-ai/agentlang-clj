# Fractl example apps

This directory contains Fractl example applications.

## Regular examples

- [Auth](auth)
- [Clickhouse logging](clickhouse)
- [Loyalty](loyalty)
- [School management](school)
- [TODO tracker](todo)


## Generative AI examples

- [Amazon Product Review Analysis](https://github.com/fractl-io/reviews-demo-app)
- [Github Issue Triage](https://github.com/fractl-io/github-issues-demo)
  - Requires [github-issues-resolver](https://github.com/fractl-io/github-issues-resolver)
- [Salesforce Lead Tracker](https://github.com/fractl-io/sfdc-demo-app)
  - Requires [sf-resolver](https://github.com/fractl-io/sf-resolver)
  - Requires google-sheets-resolver (included)

Before running the examples, bring up the database as follows:

```shell
$ cd dbschema
$ make up
```

Default Postgres database credentials:

- Host: `localhost`
- Port: `5432`
- Database: `inference`
- Username: `inference`
- Password: `password`
