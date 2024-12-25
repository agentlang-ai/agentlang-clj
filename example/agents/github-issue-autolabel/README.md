# github-issue-autolabel

This app fetches Github issues and updates triage labels periodically.

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


## License

Copyright © 2024 Fractl, Inc
