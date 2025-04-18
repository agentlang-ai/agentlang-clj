# dbschema/postgres

PostgreSQL DB schema and local development support.

## Local Development

First-time setup (until you have changes in [sql](sql) folder):

```bash
$ make build  # or 'make rebuild' later
```

Subsequently, whenever you want to run the Postgres container:

```bash
$ make run  # press Ctrl+C to stop DB container
```

### Postgres Logs

The Postgres logs are placed in the [pg/logs](pg/logs) folder
after the container is started.

### Database Seeding

The Data-definition Language (DDL) files are placed under the
[sql](sql) folder. Please maintain sequence order when naming
the SQL scripts - Postgres executs them in alphabetical order.

When you run the Postgres container for the fist time, it loads
seeding data from SQL files to bootstrap the database. Once the
DB is seeded, it detects the data and avoids any seeding again.

#### Re-seeding

If you have changes in the [sql](sql) folder content, you need
to re-seed the database with the following steps:

1. Stop the container
2. Delete the [pg](pg) folder
3. Start the conatiner (triggers fresh seeding)

#### Seed transfer

All Postgres data is stored in the [pg/data](pg/data) folder
after the container is started. Once the container is stopped
you can copy the folder content (e.g. as a tarball) to another
machine and start the container to reflect all the copied data.
