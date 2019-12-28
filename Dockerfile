FROM node:8.2.1 as client
COPY ["package.json", "yarn.lock", "/src/"]
WORKDIR /src
RUN yarn install --frozen-lockfile
COPY ["js/", "/src/js"]
RUN yarn run build

FROM haskell:8.8 as server
RUN apt-get update && env DEBIAN_FRONTEND='noninteractive' apt-get install -y \
 libpq-dev \
 && rm -rf /var/lib/apt/lists/*
RUN cabal v2-update
RUN cabal v2-install --enable-split-sections hpack
WORKDIR /src
COPY ["package.yaml", "cabal.project.local", "/src/"]
RUN hpack
RUN cabal v2-build --only-dependencies --enable-split-sections
RUN mkdir /dist
COPY [".", "/src/"]
COPY --from=client ["/src/static/js", "/src/static/js/"]
RUN cabal v2-build
RUN find dist-newstyle/build/x86_64-linux -type f -perm -u=x -print0 \
        | xargs -0 cp -t /dist2

FROM debian:stretch-slim
RUN apt-get update && env DEBIAN_FRONTEND='noninteractive' apt-get install -y \
 libpq5 \
 libgmp10 \
 && rm -rf /var/lib/apt/lists/*
COPY --from=server ["/dist2/isaacranks", "/dist2/load-data", "/dist2/rebuildranks", "/usr/local/bin/"]
