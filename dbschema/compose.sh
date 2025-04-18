#!/usr/bin/env bash

set -e
#set -x # uncomment for verbose execution

PG_CTR_NAME="inference-db" # Docker container name
PG_IMG_NAME="inference-db" # Docker image name
PG_DB_NAME="inference"     # Postgres DB name
PG_DB_USERNAME="inference" # Postgres DB username
PG_DB_PASSWORD="password"  # Postgres DB password
PG_DOCKER_UID=$(id -u)     # Postgres Docker User ID
PG_DOCKER_GID=$(id -g)     # Postgres Docker Group ID
PG_DOCKER_USER=$(id -u)    # System group ID for Postgres containr
PG_DOCKER_GROUP=$(id -g)   # System user ID for Postgres containr
PG_HOST_DIR="$(pwd)/pg"    # Host Postgres directory
CH_VERSION="24.6"          # Clickhouse version

REDIS_DATA_DIR="$(pwd)/redis"

# Print to STDERR
function err {
  printf >&2 "$1\n"
}

function errExit {
  printf >&2 "$1\n"
  exit 1
}

function getPostgresContainerId() {
  docker ps -a | grep $PG_CTR_NAME | tr -s ' ' | cut -d ' ' -f 1
}

function getPostgresImageId() {
  docker images | grep $PG_IMG_NAME | tr -s ' ' | cut -d ' ' -f 3
}

function cleanPostgresImage() {
  containerId=$(getPostgresContainerId)
  if [ ! -z "$containerId" ]; then
    docker kill $containerId || true
    docker rm $containerId
  fi

  imageId=$(getPostgresImageId)
  if [ ! -z "$imageId" ]; then
    docker rmi $imageId
  fi
}

function cleanDataDirs() {
  rm -rf $PG_HOST_DIR
  rm -rf $REDIS_DATA_DIR
}

function buildPostgresImage() {
  docker build \
    --build-arg UID=$PG_DOCKER_UID \
    --build-arg GID=$PG_DOCKER_GID \
    --build-arg USER=$PG_DOCKER_USER \
    --build-arg GROUP=$PG_DOCKER_GROUP \
    -f Dockerfile \
    -t $PG_IMG_NAME .
}

function runDockerCompose() {
  if [ -z "$1" ]; then
    err "Missing argument: docekr compose <?>"
    exit 1
  fi
  env \
    IMG_NAME="$PG_IMG_NAME" \
    PG_DOCKER_USER=$PG_DOCKER_USER \
    PG_DOCKER_GROUP=$PG_DOCKER_GROUP \
    PG_DB_USERNAME=$PG_DB_USERNAME \
    PG_DB_PASSWORD=$PG_DB_PASSWORD \
    PG_DB_NAME=$PG_DB_NAME \
    PG_HOST_DIR=$PG_HOST_DIR \
    REDIS_DATA_DIR=$REDIS_DATA_DIR \
    CHVER=$CH_VERSION \
    docker compose $1
}

function up() {
  if [ -z $(getPostgresImageId) ]; then
    buildPostgresImage
  fi
  mkdir -p ${PG_HOST_DIR}/data
  mkdir -p ${PG_HOST_DIR}/logs
  mkdir -p ${REDIS_DATA_DIR}
  # Check if macOS patch is applied
  if [ ! -f "/etc/issue" ]; then
    # assume macOS
    if [[ "$(cat docker-compose.yml | grep user:)" =~ ^[[:blank:]]*user:.+$ ]]; then
      err "On macOS, please comment out the following line in 'docker-compose.yml' file:"
      err '# user: "$PG_DOCKER_USER:$PG_DOCKER_GROUP"'
      exit 1
    fi
  fi
  runDockerCompose up
}

function down() {
  runDockerCompose down
}

function kafkaBash() {
  docker compose exec kafka /bin/bash
}

function kafkaListTopics() {
  docker compose exec kafka kafka-topics.sh \
    --list --bootstrap-server=localhost:9092
}

function kafkaCreateTopic() {
  [ -z "$1" ] && errExit "Missing argument: Kafka topic name"
  echo "Creating Kafta topic: $1"
  docker compose exec kafka kafka-topics.sh \
    --create --topic "$1" --partitions 1 \
    --replication-factor 1 --if-not-exists \
    --bootstrap-server localhost:9092
}

function kafkaDescribeTopic() {
  [ -z "$1" ] && errExit "Missing argument: Kafka topic name"
  echo "Describing Kafta topic: $1"
  docker compose exec kafka kafka-topics.sh \
    --describe --topic "$1" \
    --bootstrap-server localhost:9092
}

function kafkaProduceMessage() {
  [ -z "$1" ] && errExit "Missing argument: Kafka topic name"
  err "Producing message to Kafka topic: $1"
  docker compose exec kafka kafka-console-producer.sh \
    --topic "$1" --bootstrap-server localhost:9092
}

function kafkaConsumeMessages() {
  [ -z "$1" ] && errExit "Missing argument: Kafka topic name"
  err "Consuming messages from Kafka topic: $1"
  docker compose exec kafka kafka-console-consumer.sh \
    --topic "$1" --from-beginning --bootstrap-server localhost:9092
}

function help() {
  err "Syntax: $0 [clean | distclean | build | up | down]"
  err ""
  err "Syntax: $0 kafka-bash | kafka-list-topics"
  err ""
  err "Syntax: $0 (kafka-create-topic | kafka-describe-topic) <topic-name>"
  err ""
  err "Syntax: $0 (kafka-produce-message | kafka-consume-messages) <topic-name>"
}

case "$1" in
clean) cleanPostgresImage ;;
distclean) cleanPostgresImage
           cleanDataDirs ;;
build) buildPostgresImage ;;
up) up ;;
down) down ;;
kafka-bash) kafkaBash ;;
kafka-list-topics) kafkaListTopics ;;
kafka-create-topic) kafkaCreateTopic "$2" ;;
kafka-describe-topic) kafkaDescribeTopic "$2" ;;
kafka-produce-message) kafkaProduceMessage "$2" ;;
kafka-consume-messages) kafkaConsumeMessages "$2" ;;
*) help ;;
esac
