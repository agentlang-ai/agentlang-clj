#!/bin/bash
set -e 
clickhouse client -n <<-EOSQL
CREATE TABLE fractldb.events (
  app_uuid UUID,
  event_uuid UUID,
  event_generated_ts DateTime('UTC'),
  event_processed_ts DateTime('UTC'),
  event_details String)
  ENGINE = MergeTree
  ORDER BY (app_uuid, event_generated_ts);
EOSQL
