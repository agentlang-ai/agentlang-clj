#!/usr/bin/env bash

set -e
#set -x # uncomment for verbose execution

CTR_NAME="inference-db"      # Docker container name
IMG_NAME="inference-db"      # Docker image name
DB_NAME="inference"          # Postgres DB name
DB_USERNAME="inference"      # Postgres DB username
DB_PASSWORD="password"       # Postgres DB password
PG_DATA_DIR="$(pwd)/pg/data" # Postgres data directory
PG_LOGS_DIR="$(pwd)/pg/logs" # Postgres logs directory
DOCKER_USER=$(id -un)        # System user for PG dirs
DOCKER_GROUP=$(id -gn)       # System group for PG dirs
DOCKER_UID=$(id -u)
DOCKER_GID=$(id -g)

# Print to STDERR
function err {
  printf >&2 "$1\n"
}

function getContainerId() {
  docker ps -a | grep $CTR_NAME | tr -s ' ' | cut -d ' ' -f 1
}

function getImageId() {
  docker images | grep $IMG_NAME | tr -s ' ' | cut -d ' ' -f 3
}

function clean() {
  containerId=$(getContainerId)
  if [ ! -z "$containerId" ]; then
    docker kill $containerId || true
    docker rm $containerId
  fi

  imageId=$(getImageId)
  if [ ! -z "$imageId" ]; then
    docker rmi $imageId
  fi
}

function cleandata() {
  rm -rf "$PG_DATA_DIR"
  rm -rf "$PG_LOGS_DIR"
}

function build() {
  docker build \
    --build-arg UID=$DOCKER_UID \
    --build-arg GID=$DOCKER_GID \
    --build-arg USER=$DOCKER_USER \
    --build-arg GROUP=$DOCKER_GROUP \
    -t $IMG_NAME .
}

function run() {
  ##
  # Following env vars are supported:
  # ---
  # POSTGRES_PASSWORD
  # POSTGRES_USER
  # PGDATA
  # POSTGRES_DB
  # POSTGRES_INITDB_ARGS
  ##
  [ -d "$PG_DATA_DIR" ] || mkdir -p "$PG_DATA_DIR"
  [ -d "$PG_LOGS_DIR" ] || mkdir -p "$PG_LOGS_DIR"
  containerId=$(getContainerId)
  if [ -z "$containerId" ]; then
    if [ -z $(getImageId) ]; then
      err "Docker image not found. Build the image first."
      exit 1
    fi
    docker run \
      -p "0.0.0.0:5432:5432" \
      --name "$CTR_NAME" \
      --user $DOCKER_USER \
      -e POSTGRES_USER=$DB_USERNAME \
      -e POSTGRES_PASSWORD=$DB_PASSWORD \
      -e POSTGRES_DB=$DB_NAME \
      -v $PG_DATA_DIR:/var/lib/postgresql/data \
      -v $PG_LOGS_DIR:/var/log/postgresql \
      $IMG_NAME
  else
    docker start -i $containerId
  fi
}

function help() {
  err "Syntax: $0 [clean | build | run]"
}

case "$1" in
clean) clean ;;
cleandata) cleandata ;;
build) build ;;
run) run ;;
*) help ;;
esac
