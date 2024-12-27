# github-issue-autolabel

This app fetches Github open issues for a given repo (via `config.edn`) and updates triage labels periodically.

## Usage

Copy `run.sh.template` as `run.sh` and edit the values suitably.

```shell
$ cp run.sh.template run.sh
$ # Edit run.sh values
$ chmod a+x run.sh
```

Note:
- To create a Github token visit https://github.com/settings/tokens?type=beta and create a token with permissions to access repository issues.
Then run the app:

```shell
$ ./run.sh
```

Please follow the instructions in the `run.sh` script to run the app.

## License

Copyright © 2024 Fractl, Inc
