FROM haskell:8.0

RUN mkdir -p /var/src/terrarium

WORKDIR /var/src/terrarium
COPY . /var/src/terrarium
RUN stack setup
RUN stack install
