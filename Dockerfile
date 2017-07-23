FROM node:8.2.1 as client
COPY ["package.json", "yarn.lock", "/src/"]
WORKDIR /src
RUN yarn install --frozen-lockfile
COPY ["js/", "/src/js"]
RUN yarn run build

FROM fpco/stack-build:lts-8.20 as server
COPY ["stack.yaml", "/src/"]
WORKDIR /src
RUN stack setup
COPY ["package.yaml", "/src/"]
RUN stack build --only-dependencies
RUN mkdir /dist
COPY [".", "/src/"]
COPY --from=client ["/src/static/js", "/src/static/js/"]
RUN stack --local-bin-path /dist build --copy-bins

FROM debian:stretch
RUN apt-get update && env DEBIAN_FRONTEND='noninteractive' apt-get install -y \
 libpq5 \
 && rm -rf /var/lib/apt/lists/*
COPY --from=server ["/dist/isaacranks", "/dist/load-data", "/dist/rebuildranks", "/usr/local/bin/"]
