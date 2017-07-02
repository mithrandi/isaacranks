FROM debian:stretch
RUN apt-get update && env DEBIAN_FRONTEND='noninteractive' apt-get install -y \
 libpq5 \
 && rm -rf /var/lib/apt/lists/*
COPY ["dist/isaacranks", "dist/load-data", "dist/rebuildranks", "/usr/local/bin/"]
