#!/usr/bin/env bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# dijkstra-devnet-test.sh
#
#   Start a local devnet in the Dijkstra era, with Ogmios on it. Build a
#   transaction with a sub-transaction, but do not submit it. Then run
#   COMMAND, with DIJKSTRA_TX_CBOR set to the transaction (in base16), and
#   DIJKSTRA_TX_ID and DIJKSTRA_SUB_TX_ID set to the ids of both, and
#   DIJKSTRA_PRODUCER_PID set to the process id of the node that makes the
#   blocks, and DIJKSTRA_PROTOCOL_PARAMETERS set to a file with the protocol
#   parameters from cardano-cli, and DIJKSTRA_GENESIS set to the Dijkstra
#   genesis file of the devnet. Ogmios is connected to the other node, a
#   relay.
#   Everything started here stops when the script ends. The files of the
#   devnet are removed, unless the script fails.
#
#   Needs cardano-testnet, cardano-node (11.2.0 or later), cardano-cli, jq and
#   curl on the PATH. Ogmios listens on port 1337.
#
# Usage: ./dijkstra-devnet-test.sh OGMIOS_BIN COMMAND [ARGS...]

set -euo pipefail

if [ "$#" -lt 2 ]; then
  echo "Usage: $0 OGMIOS_BIN COMMAND [ARGS...]"
  exit 1
fi
OGMIOS_BIN=$(realpath "$1")
shift

MAGIC=42
FEE=1000000
# A short path: the node socket path must stay short.
WORK=$(mktemp -d /tmp/devnet.XXXXXX)
# node1 makes the blocks, node2 is a relay.
SOCKET="$WORK/env/socket/node2/sock"
PIDS=()

# Whether the processes started here have all exited.
none_running() {
  local pid
  for pid in "${PIDS[@]}" "$(cat "$WORK/env/logs/node1/node.pid" 2>/dev/null)" "$(cat "$WORK/env/logs/node2/node.pid" 2>/dev/null)"; do
    if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
      return 1
    fi
  done
}

stop() {
  local status=$?
  for pid in "${PIDS[@]}"; do
    kill "$pid" 2>/dev/null || true
  done
  for node in node1 node2; do
    if [ -f "$WORK/env/logs/$node/node.pid" ]; then
      # A paused node only stops once it is resumed.
      kill -CONT "$(cat "$WORK/env/logs/$node/node.pid")" 2>/dev/null || true
      kill "$(cat "$WORK/env/logs/$node/node.pid")" 2>/dev/null || true
    fi
  done
  if [ "$status" -eq 0 ]; then
    # Wait for them to exit, or files they still write could stay behind.
    wait_for 30 none_running || true
    rm -rf "$WORK" 2>/dev/null || true
  else
    echo "The devnet files are in $WORK."
  fi
}
trap stop EXIT

fail() {
  echo "error: $1"
  tail -n 20 "$WORK"/*.log
  exit 1
}

# wait_for SECONDS COMMAND...: run COMMAND every 2 seconds until it succeeds.
wait_for() {
  local deadline=$((SECONDS + $1))
  shift
  until "$@" > /dev/null 2>&1; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 2
  done
}

cli() {
  cardano-cli "$@" --testnet-magic "$MAGIC" --socket-path "$SOCKET"
}

in_dijkstra() {
  [ "$(cli query tip | jq -r .era)" = "Dijkstra" ]
}

ogmios_connected() {
  curl -sSf http://127.0.0.1:1337/health | grep -q '"connectionStatus":"connected"'
}

# The UTxO with the most lovelace at the given address.
largest_utxo() {
  cli query utxo --address "$1" --output-json | jq -r 'to_entries | max_by(.value.value.lovelace) | .key'
}

lovelace_of() {
  cli query utxo --tx-in "$1" --output-json | jq -r '.[].value.lovelace'
}

echo "Starting the devnet in $WORK."
cardano-testnet create-env --nodes spo,relay --testnet-magic "$MAGIC" --output "$WORK/env" \
  > "$WORK/create-env.log" 2>&1 || fail "cardano-testnet create-env failed"
# Conway from the start, Dijkstra from epoch 1. Dijkstra at epoch 0 stops the node.
jq '.Configuration.Testing.TestDijkstraHardForkAtEpoch = 1' "$WORK/env/configuration.yaml" > "$WORK/config.json"
mv "$WORK/config.json" "$WORK/env/configuration.yaml"
# The test pauses the block producer, so the chain-stall watchdog is turned off.
(cd "$WORK" && exec cardano-testnet cardano --node-env "$WORK/env" --disable-chain-stall-watchdog > "$WORK/testnet.log" 2>&1) &
PIDS+=($!)

wait_for 300 cli query tip || fail "the node does not answer"
echo "Waiting for the Dijkstra era."
wait_for 600 in_dijkstra || fail "the chain did not reach the Dijkstra era"

"$OGMIOS_BIN" --node-socket "$SOCKET" --node-config "$WORK/env/configuration.yaml" \
  --host 127.0.0.1 --port 1337 > "$WORK/ogmios.log" 2>&1 &
PIDS+=($!)
wait_for 120 ogmios_connected || fail "Ogmios did not connect to the node"

# The sub-transaction moves a UTxO of utxo1. utxo1 signs it and is its guard.
# The top-level transaction needs utxo1 too, as a required signer. It moves a
# UTxO of utxo2, which pays the fee.
KEYS="$WORK/env/utxo-keys"
ADDR1=$(cat "$KEYS/utxo1/utxo.addr")
ADDR2=$(cat "$KEYS/utxo2/utxo.addr")
KEY_HASH1=$(cardano-cli address key-hash --payment-verification-key-file "$KEYS/utxo1/utxo.vkey")
SUB_IN=$(largest_utxo "$ADDR1")
TOP_IN=$(largest_utxo "$ADDR2")

cardano-cli dijkstra sub-transaction build-raw \
  --tx-in "$SUB_IN" \
  --tx-out "$ADDR1+$(lovelace_of "$SUB_IN")" \
  --guard-key-hash "$KEY_HASH1" \
  --top-level-guard-key-hash "$KEY_HASH1" \
  --out-file "$WORK/sub.body.json"
cardano-cli dijkstra sub-transaction sign \
  --sub-tx-file "$WORK/sub.body.json" \
  --signing-key-file "$KEYS/utxo1/utxo.skey" \
  --out-file "$WORK/sub.signed.json"
cardano-cli dijkstra transaction build-raw \
  --tx-in "$TOP_IN" \
  --tx-out "$ADDR2+$(($(lovelace_of "$TOP_IN") - FEE))" \
  --required-signer-hash "$KEY_HASH1" \
  --fee "$FEE" \
  --signed-sub-tx-file "$WORK/sub.signed.json" \
  --out-file "$WORK/body.json"
cardano-cli dijkstra transaction sign \
  --tx-body-file "$WORK/body.json" \
  --signing-key-file "$KEYS/utxo2/utxo.skey" \
  --signing-key-file "$KEYS/utxo1/utxo.skey" \
  --testnet-magic "$MAGIC" \
  --out-file "$WORK/signed.json"

DIJKSTRA_SUB_TX_ID=$(cardano-cli dijkstra sub-transaction txid --signed-sub-tx-file "$WORK/sub.signed.json" --output-text)
DIJKSTRA_TX_ID=$(cardano-cli dijkstra transaction txid --tx-file "$WORK/signed.json" --output-text)
DIJKSTRA_TX_CBOR=$(jq -r .cborHex "$WORK/signed.json")
DIJKSTRA_PRODUCER_PID=$(cat "$WORK/env/logs/node1/node.pid")
DIJKSTRA_PROTOCOL_PARAMETERS="$WORK/protocol-parameters.json"
cli dijkstra query protocol-parameters --out-file "$DIJKSTRA_PROTOCOL_PARAMETERS"
DIJKSTRA_GENESIS="$WORK/env/dijkstra-genesis.json"
export DIJKSTRA_SUB_TX_ID DIJKSTRA_TX_ID DIJKSTRA_TX_CBOR DIJKSTRA_PRODUCER_PID DIJKSTRA_PROTOCOL_PARAMETERS DIJKSTRA_GENESIS
echo "Built transaction $DIJKSTRA_TX_ID with sub-transaction $DIJKSTRA_SUB_TX_ID."

"$@"
