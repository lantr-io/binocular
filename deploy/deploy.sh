#!/usr/bin/env bash
# Build the Binocular fat jar on this machine and ship it to the watchtower box.
#
#   deploy/deploy.sh user@host --v2                 # build + copy jar + restart binocular-bridge-v2
#   deploy/deploy.sh user@host --v2 --with-config   # also copy application-preprod-v2.conf
#   deploy/deploy.sh user@host --v2 --no-restart    # stage the jar without bouncing the service
#   deploy/deploy.sh user@host                      # the RETIRED v1 target (see below)
#   deploy/deploy.sh user@host --no-build           # skip sbt assembly, ship the existing jar
#
# --v2 is the live deployment: binocular-v2.jar / application-preprod-v2.conf / binocular-bridge-v2,
# one `watchtower` process (oracle + relay + confirm + proofs). The flagless form still targets the
# retired v1 triple (binocular.jar / application-preprod.conf / binocular-watchtower) so a rollback
# needs no argument-juggling; drop it once v1's unit is deleted for good.
#
# The jar, config, and secrets live in /var/lib/binocular on the box (out of the Nix store).
# Secrets (secrets.env) are NOT deployed by this script — copy them once by hand:
#   scp deploy/secrets.env.example user@host:/tmp/secrets.env   # then edit + install mode 600
set -euo pipefail

HOST="${1:-}"
if [[ -z "$HOST" ]]; then
    echo "usage: $0 user@host [--v2] [--with-config] [--no-build] [--no-restart]" >&2
    exit 1
fi
shift

WITH_CONFIG=0
BUILD=1
RESTART=1
V2=0
for arg in "$@"; do
    case "$arg" in
        --v2) V2=1 ;;
        --with-config) WITH_CONFIG=1 ;;
        --no-build) BUILD=0 ;;
        --no-restart) RESTART=0 ;;
        *) echo "unknown option: $arg" >&2; exit 1 ;;
    esac
done

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# One build artifact, renamed at install time: build.sbt has a single assemblyJarName.
JAR="$ROOT/target/out/jvm/scala-3.3.8/binocular/binocular.jar"
STATE_DIR="/var/lib/binocular"

if [[ "$V2" == "1" ]]; then
    JAR_NAME="binocular-v2.jar"
    CONFIG="application-preprod-v2.conf"
    UNIT="binocular-bridge-v2"
else
    JAR_NAME="binocular.jar"
    CONFIG="application-preprod.conf"
    UNIT="binocular-watchtower"
fi

if [[ "$BUILD" == "1" ]]; then
    echo "==> Building fat jar (sbt assembly)"
    (cd "$ROOT" && sbt -batch assembly)
fi

if [[ ! -f "$JAR" ]]; then
    echo "jar not found at $JAR — run without --no-build, or fix the Scala version in this script" >&2
    exit 1
fi

echo "==> Staging jar to $HOST:/tmp/$JAR_NAME"
scp "$JAR" "$HOST:/tmp/$JAR_NAME"
if [[ "$WITH_CONFIG" == "1" ]]; then
    scp "$ROOT/$CONFIG" "$HOST:/tmp/$CONFIG"
fi

if [[ "$RESTART" == "1" ]]; then
    RESTART_CMD="sudo systemctl restart $UNIT && sudo systemctl --no-pager --lines=0 status $UNIT"
else
    RESTART_CMD="echo '==> --no-restart: $UNIT left running the OLD jar until you restart it'"
fi

echo "==> Installing into $STATE_DIR (sudo on the box)"
# shellcheck disable=SC2087
ssh "$HOST" "sudo install -o binocular -g binocular -m 644 /tmp/$JAR_NAME $STATE_DIR/$JAR_NAME && \
    if [ -f /tmp/$CONFIG ]; then sudo install -o binocular -g binocular -m 644 /tmp/$CONFIG $STATE_DIR/$CONFIG && rm -f /tmp/$CONFIG; fi && \
    rm -f /tmp/$JAR_NAME && \
    $RESTART_CMD"

echo "==> Done. Watch logs with:  ssh $HOST 'journalctl -fu $UNIT -o cat'"
