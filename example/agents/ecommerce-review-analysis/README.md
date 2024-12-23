# ecommerce-review-analysis

This app analyzes customer-reviews for E-commerce products.

Data source: https://www.kaggle.com/datasets/cynthiarempel/amazon-us-customer-reviews-dataset?resource=download

## Usage

Start the app:

```shell
$ export OPENAI_API_KEY="<FIXME>"
$ agent run
```

Alternatively, instead of `agent run` you may use Docker to run the app:

```shell
$ docker run --rm \
  -p "0.0.0.0:8080:8080" \
  -v $PWD:/agentlang \
  -e OPENAI_API_KEY="$OPENAI_API_KEY" \
  -it agentlang/agentlang.cli:latest \
  agent run
```

In another terminal:

```shell
$ curl -X POST http://localhost:8080/api/EcommerceReviewAnalysiss.Core/Review \
  -H 'Content-type: application/json' \
  --data-binary @sample.json
```

After the request is submitted successfully, you may inspect the analysis as follows:

```shell
$ curl http://localhost:8080/api/EcommerceReviewAnalysiss.Core/Analysis
```

## License

Copyright © 2024 Fractl, Inc

