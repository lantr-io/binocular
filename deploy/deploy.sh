#!/usr/bin/env bash
# Build the Binocular fat jar on this machine and ship it to the watchtower box.
#
#   deploy/deploy.sh user@host                 # build + copy jar + restart service
#   deploy/deploy.sh user@host --with-config   # also copy application-preprod.conf (first deploy)
#   deploy/deploy.sh user@host --no-build      # skip sbt assembly, ship the existing jar
#
# The jar, config, and secrets live in /var/lib/binocular on the box (out of the Nix store).
# Secrets (secrets.env) are NOT deployed by this script — copy them once by hand:
#   scp deploy/secrets.env.example user@host:/tmp/secrets.env   # then edit + install mode 600
set -euo pipefail

HOST="${1:-}"
if [[ -z "$HOST" ]]; then
    echo "usage: $0 user@host [--with-config] [--no-build]" >&2
    exit 1
fi
shift

WITH_CONFIG=0
BUILD=1
for arg in "$@"; do
    case "$arg" in
        --with-config) WITH_CONFIG=1 ;;
        --no-build) BUILD=0 ;;
        *) echo "unknown option: $arg" >&2; exit 1 ;;
    esac
done

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
JAR="$ROOT/target/out/jvm/scala-3.3.8/binocular/binocular.jar"
STATE_DIR="/var/lib/binocular"
CONFIG="application-preprod.conf"

if [[ "$BUILD" == "1" ]]; then
    echo "==> Building fat jar (sbt assembly)"
    (cd "$ROOT" && sbt -batch assembly)
fi

if [[ ! -f "$JAR" ]]; then
    echo "jar not found at $JAR — run without --no-build, or fix the Scala version in this script" >&2
    exit 1
fi

echo "==> Staging jar to $HOST:/tmp"
scp "$JAR" "$HOST:/tmp/binocular.jar"
if [[ "$WITH_CONFIG" == "1" ]]; then
    scp "$ROOT/$CONFIG" "$HOST:/tmp/$CONFIG"
fi

echo "==> Installing into $STATE_DIR and restarting service (sudo on the box)"
# shellcheck disable=SC2087
ssh "$HOST" "sudo install -o binocular -g binocular -m 644 /tmp/binocular.jar $STATE_DIR/binocular.jar && \
    if [ -f /tmp/$CONFIG ]; then sudo install -o binocular -g binocular -m 644 /tmp/$CONFIG $STATE_DIR/$CONFIG && rm -f /tmp/$CONFIG; fi && \
    rm -f /tmp/binocular.jar && \
    sudo systemctl restart binocular-watchtower && \
    sudo systemctl --no-pager --lines=0 status binocular-watchtower"

echo "==> Done. Watch logs with:  ssh $HOST 'journalctl -fu binocular-watchtower -o cat'"
