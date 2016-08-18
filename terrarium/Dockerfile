FROM ubuntu:trusty

# install required packages
RUN apt-get update && apt-get install -y libgmp3-dev libpq-dev
COPY dist/x86_64-linux/terrarium-api-exe /usr/bin
COPY dist/x86_64-linux/postgres-migration-exe /usr/bin
COPY migrations /migrations
CMD ["terrarium-api-exe", "8000"]
