#FROM postgres:16.1-bookworm
FROM pgvector/pgvector:0.6.0-pg16
ARG UID
ARG GID
ARG USER
ARG GROUP

RUN echo "UID $UID, GID $GID, USER $USER, GROUP $GROUP"

RUN groupadd -g $GID $GROUP || true
RUN useradd -u $UID -g $GID $USER

COPY sql/*.sql /docker-entrypoint-initdb.d/
