# Build stage
FROM ubuntu as build

WORKDIR /opt/build

RUN apt-get update && \
    apt-get install -y wget \
    libjansson-dev \
    libjwt-dev \
    gnutls-dev \
    libpq-dev \
    libtinfo-dev

RUN wget -qO- https://get.haskellstack.org/ | sh
RUN export PATH=$(stack path --local-bin):$PATH

COPY stack.yaml some-api.cabal ./
RUN stack build --only-dependencies --system-ghc --fast

COPY . .
RUN stack install

# Run stage
FROM ubuntu

WORKDIR /app

RUN apt-get update && \
    apt-get install -y libjansson-dev \
    libjwt-dev \
    gnutls-dev \
    libpq-dev \
    ca-certificates

COPY --from=build /root/.local/bin/some-api-exe .

ARG HMAC_SECRET
ENV HMAC_SECRET=$HMAC_SECRET
ARG GOOGLE_MAIL
ENV GOOGLE_MAIL=$GOOGLE_MAIL
ARG GOOGLE_PASS
ENV GOOGLE_PASS=$GOOGLE_PASS
ARG POSTGRES_USER
ENV POSTGRES_USER=$POSTGRES_USER
ARG POSTGRES_PASSWORD
ENV POSTGRES_PASSWORD=$POSTGRES_PASSWORD
ARG POSTGRES_DB
ENV POSTGRES_DB=$POSTGRES_DB

CMD "/app/some-api-exe"
