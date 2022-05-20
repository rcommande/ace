FROM alpine:latest

RUN apk add --no-cache pkgconfig yarn make m4 git gcc g++ musl-dev perl perl-utils libbz2 zlib zlib-dev zlib-static autoconf automake bzip2-dev bzip2-static opam bash npm openssl-dev

RUN npm install -g esy
ADD . /app
WORKDIR /app
RUN esy
