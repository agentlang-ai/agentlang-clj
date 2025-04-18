#!/usr/bin/env bash

set -e

CTR_NAME="fractl"
IMG_NAME="fractl"

PORT="8005"
POSTGRES_HOST="localhost"
POSTGRES_PORT="5432"
POSTGRES_DB="inference"
POSTGRES_USER="inference"
POSTGRES_PASSWORD="password"

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


function build() {
    docker build --no-cache \
      -t $IMG_NAME .
}


function run() {
    containerId=$(getContainerId)
    if [ -z "$containerId" ]; then
        if [ -z $(getImageId) ]; then
            err "Docker image not found. Build the image first."
            exit 1
        fi
        docker run \
          -p "0.0.0.0:$PORT:$PORT" \
          --name "$CTR_NAME" \
          -e POSTGRES_HOST=$POSTGRES_HOST \
          -e POSTGRES_PORT=$POSTGRES_PORT \
          -e POSTGRES_USER=$POSTGRES_USER \
          -e POSTGRES_PASSWORD=$POSTGRES_PASSWORD \
          -e POSTGRES_DB=$POSTGRES_DB \
          -e OPENAI_API_KEY=$OPENAI_API_KEY \
          -v $PWD:/fractl \
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
build) build ;;
run) run ;;
*) help ;;
esac
