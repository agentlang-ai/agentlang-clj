FROM clojure:temurin-21-lein-jammy

ENV DOCKER_CONTAINER=Yes

COPY bin/fractl /usr/local/bin/

RUN apt update && apt install -y git wget iproute2
RUN mkdir -p ~/.lein
RUN fractl

# For development versions
WORKDIR /tmp
RUN git clone https://github.com/fractl-io/fractl \
  && cd fractl \
  && lein install \
  && lein uberjar \
  && cp target/fract*-standalone.jar ~/.fractl/self-installs


WORKDIR /fractl

CMD ["fractl", "run", "-c", "config.edn"]
