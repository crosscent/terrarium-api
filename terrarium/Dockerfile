FROM haskell:8.0

# perform psql migration
RUN apt-get update && apt-get install -y libpq-dev
RUN mkdir -p /var/src/haskell-postgres-migration
WORKDIR /var/src/haskell-postgres-migration
RUN git clone https://github.com/crosscent/haskell_postgres_migration.git .
RUN rm -rf migrations/
RUN stack setup
RUN stack install

RUN mkdir -p /var/src/terrarium
WORKDIR /var/src/terrarium
COPY . /var/src/terrarium
RUN stack setup
RUN stack install
