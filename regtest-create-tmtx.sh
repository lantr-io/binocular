#!/usr/bin/env bash
# Mine regtest funds, craft a BTC tx, and submit it as a TMTx on Cardano.
#
# Requires: bitcoin-cli, sbt. Assumes bitcoind (regtest) and any Cardano
# backend (preprod Blockfrost by default) are already running/reachable.
#
# Secrets come from env / flags, not this script:
#   BITCOIN_CONF        path to bitcoin.conf (e.g. /Users/otto/work/btc/bitcoin.conf)
#   BINOCULAR_CONFIG    path to binocular HOCON (default: application-regtest.conf)
#   BTC_WALLET          regtest wallet name     (default: binocular-regtest)
#   BTC_SEND_AMOUNT     BTC amount to send      (default: 0.001)

set -euo pipefail

: "${BITCOIN_CONF:?set BITCOIN_CONF to your bitcoin.conf path}"
BINOCULAR_CONFIG="${BINOCULAR_CONFIG:-application-regtest.conf}"
BTC_WALLET="${BTC_WALLET:-binocular-regtest}"
BTC_SEND_AMOUNT="${BTC_SEND_AMOUNT:-0.001}"

bcli() { bitcoin-cli -conf="$BITCOIN_CONF" -regtest "$@"; }
wcli() { bitcoin-cli -conf="$BITCOIN_CONF" -regtest -rpcwallet="$BTC_WALLET" "$@"; }

echo ">> Ensuring wallet '$BTC_WALLET' exists and is loaded"
if ! bcli listwallets | grep -q "\"$BTC_WALLET\""; then
    bcli loadwallet "$BTC_WALLET" >/dev/null 2>&1 \
        || bcli createwallet "$BTC_WALLET" >/dev/null
fi

MINE_ADDR="$(wcli getnewaddress)"
BALANCE="$(wcli getbalance)"
# Need ~101 confirmations on a coinbase before it's spendable.
if awk "BEGIN{exit !($BALANCE < $BTC_SEND_AMOUNT)}"; then
    echo ">> Balance $BALANCE BTC too low; mining 101 blocks to $MINE_ADDR"
    bcli generatetoaddress 101 "$MINE_ADDR" >/dev/null
else
    echo ">> Balance $BALANCE BTC sufficient; mining 1 block to refresh tip"
    bcli generatetoaddress 1 "$MINE_ADDR" >/dev/null
fi

DEST_ADDR="$(wcli getnewaddress)"
echo ">> Sending $BTC_SEND_AMOUNT BTC -> $DEST_ADDR"
TXID="$(wcli sendtoaddress "$DEST_ADDR" "$BTC_SEND_AMOUNT")"
echo "   txid: $TXID"

# verbose=0 returns raw hex (works even for mempool txs).
TX_HEX="$(bcli getrawtransaction "$TXID" 0)"
echo ">> Raw tx hex (${#TX_HEX} chars)"

echo ">> Submitting create-tmtx via binocular CLI"
exec sbt --error "run --config $BINOCULAR_CONFIG create-tmtx $TX_HEX"
