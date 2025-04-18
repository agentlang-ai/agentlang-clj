-- This is the init SQL script

-- AWS RDS (PostgreSQL) Supported extensions
-- https://docs.aws.amazon.com/AmazonRDS/latest/PostgreSQLReleaseNotes/postgresql-extensions.html

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE EXTENSION citext;  -- Case-insentitive text type
CREATE DOMAIN email AS citext
  CHECK ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );

CREATE EXTENSION IF NOT EXISTS "vector";
