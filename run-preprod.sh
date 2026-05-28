#!/bin/bash
# Run the binocular CLI against preprod with a selectable oracle profile.
#   ./run-preprod.sh synced <subcommand...>   # shared oracle (preprod-synced.conf)
#   ./run-preprod.sh local  <subcommand...>   # our fresh oracle (preprod-local.conf)
# Shared secrets + network/bitcoin come from .env (gitignored).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROFILE="${1:?usage: run-preprod.sh <synced|local> <subcommand...>}"; shift
case "$PROFILE" in
  synced) CONF="$HERE/preprod-synced.conf" ;;
  local)  CONF="$HERE/preprod-local.conf" ;;
  *) echo "unknown profile '$PROFILE' (want: synced | local)" >&2; exit 2 ;;
esac
set -a; source "$HERE/.env"; set +a
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
export PATH="$JAVA_HOME/bin:/usr/local/bin:/usr/bin:/bin"
cd "$HERE"
exec java -Dsbt.color=false -jar "/mnt/c/Program Files (x86)/sbt/bin/sbt-launch.jar" \
  "set Compile / run / javaOptions := Seq(\"-Dconfig.file=$CONF\")" \
  "runMain binocular.main $*"
