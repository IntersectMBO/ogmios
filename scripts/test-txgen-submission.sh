#!/usr/bin/env bash
#
# Integration test: submit transactions through Ogmios with cardano-node's
# tx-generator (its `submissionEndpointProtocol: "Ogmios"` transport).
#
# Moved here from cardano-node PR #6609 (bench/tx-generator/test-ogmios.sh):
# the test exercises Ogmios's submitTransaction contract and resolves its
# binaries from this repository's flake, so this is where it belongs. Kept as
# a plain script for now; integrating it with the server/test/integration
# suite is a follow-up.
#
# Usage:
#   bash scripts/test-txgen-submission.sh [cardano-node-flake-ref]
#
# Binaries are resolved with nix:
#   ogmios        ← this repository's flake (.#ogmios)
#   cardano-*     ← this flake's cardano-node input (uses tag, not hash)
#   tx-generator  ← the cardano-node flake ref in $1
#                   (default: github:IntersectMBO/cardano-node/master)
#
# NOTE: until IntersectMBO/cardano-node#6609 is merged, master's tx-generator
# has no submission endpoint yet — pass the PR branch instead:
#   bash scripts/test-txgen-submission.sh github:IntersectMBO/cardano-node/tx-generator-ogmios-submit-mode
#
# Any binary can also be supplied directly through the environment, skipping
# its nix build: OGMIOS, CARDANO_NODE, CARDANO_CLI, CARDANO_TESTNET,
# TX_GENERATOR.
#
# The flake ref needs `?submodules=1` even for the local checkout: the server
# pulls hjsonpointer, hjsonschema and wai-routes from git submodules that its
# cabal.project needs, and without the flag the flake source omits them. For
# a remote ref, only the git fetcher honours it, e.g.
# `git+https://github.com/IntersectMBO/ogmios?submodules=1&ref=<branch>`
# (on a `github:` ref it is silently ignored).
#
set -euo pipefail

cd "$(dirname "$0")/.."

OGMIOS_FLAKE="${OGMIOS_FLAKE:-.?submodules=1}"
TXGEN_FLAKE="${1:-github:IntersectMBO/cardano-node/master}"
TESTNET_MAGIC=42
OGMIOS_PORT=11337

# --- Check host prerequisites ---
for cmd in nix jq nc; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "ERROR: required command not found: $cmd"
        exit 1
    fi
done

# --- Resolve binaries (unless supplied through the environment) ---
echo "=== Resolving nix packages ==="
echo "  ogmios flake: $OGMIOS_FLAKE"
echo "  tx-generator flake: $TXGEN_FLAKE"

# ogmios: this repository's flake
if [ -z "${OGMIOS:-}" ]; then
    OGMIOS=$(nix build "${OGMIOS_FLAKE}#ogmios" --no-link --print-out-paths)/bin/ogmios
fi

# cardano-node tools: input ref from this flake (uses tag, not hash).
# Requires the flake to declare a cardano-node input — this branch does, but
# e.g. ogmios master does not, in which case the jq below yields
# "github:null/null/null". Guard against that with a clear error.
if [ -z "${CARDANO_NODE:-}" ] || [ -z "${CARDANO_CLI:-}" ] || [ -z "${CARDANO_TESTNET:-}" ]; then
    CN_FLAKE=$(nix flake metadata "${OGMIOS_FLAKE}" --json \
      | jq -r '.locks.nodes["cardano-node"].original
               | "github:\(.owner)/\(.repo)/\(.ref)"')
    echo "  cardano-node flake: $CN_FLAKE"
    case "$CN_FLAKE" in
      *null*)
        echo "ERROR: flake '$OGMIOS_FLAKE' has no usable cardano-node input."
        exit 1
        ;;
    esac
    CARDANO_NODE="${CARDANO_NODE:-$(nix build "${CN_FLAKE}#cardano-node" --no-link --print-out-paths)/bin/cardano-node}"
    CARDANO_CLI="${CARDANO_CLI:-$(nix build "${CN_FLAKE}#cardano-cli" --no-link --print-out-paths)/bin/cardano-cli}"
    CARDANO_TESTNET="${CARDANO_TESTNET:-$(nix build "${CN_FLAKE}#cardano-testnet" --no-link --print-out-paths)/bin/cardano-testnet}"
fi

# tx-generator: from the cardano-node flake ref in $1
if [ -z "${TX_GENERATOR:-}" ]; then
    TX_GENERATOR=$(nix build "${TXGEN_FLAKE}#tx-generator" --no-link --print-out-paths)/bin/tx-generator
fi

for bin in OGMIOS CARDANO_TESTNET CARDANO_NODE CARDANO_CLI TX_GENERATOR; do
    path="${!bin}"
    if [ ! -x "$path" ]; then
        echo "ERROR: $bin not found or not executable at: $path"
        exit 1
    fi
    echo "  $bin: $path"
done

# --- Set up work directory ---
WORKDIR=$(mktemp -d /tmp/ogmios-test.XXXXXX)
LOGS_DIR="$WORKDIR/logs"
TESTNET_DIR="$WORKDIR/testnet"
mkdir -p "$LOGS_DIR"

echo ""
echo "=== Ogmios tx-generator integration test ==="
echo "Work directory: $WORKDIR"

# shellcheck disable=SC2329  # invoked via the EXIT trap only
cleanup() {
    echo ""
    echo "=== Cleaning up ==="
    [ -n "${OGMIOS_PID:-}" ] && kill "$OGMIOS_PID" 2>/dev/null && echo "Stopped ogmios (PID $OGMIOS_PID)"
    [ -n "${TESTNET_PID:-}" ] && kill "$TESTNET_PID" 2>/dev/null && echo "Stopped cardano-testnet (PID $TESTNET_PID)"
    echo "Logs available at: $LOGS_DIR"
}
trap cleanup EXIT

# --- 1. Start cardano-testnet ---
echo ""
echo "--- Starting cardano-testnet ---"
CARDANO_CLI="$CARDANO_CLI" CARDANO_NODE="$CARDANO_NODE" \
  "$CARDANO_TESTNET" cardano \
    --testnet-magic "$TESTNET_MAGIC" \
    --output-dir "$TESTNET_DIR" \
    > "$LOGS_DIR/cardano-testnet.stdout" 2> "$LOGS_DIR/cardano-testnet.stderr" &
TESTNET_PID=$!
echo "cardano-testnet PID: $TESTNET_PID"

# --- 2. Wait for node socket ---
echo "Waiting for node socket..."
SOCKET_PATH=""
for i in $(seq 1 120); do
    SOCKET_PATH=$(find "$TESTNET_DIR" -name "sock" 2>/dev/null | head -1 || true)
    if [ -n "$SOCKET_PATH" ] && [ -S "$SOCKET_PATH" ]; then
        break
    fi
    SOCKET_PATH=""
    sleep 1
done

if [ -z "$SOCKET_PATH" ]; then
    echo "FAILED: node socket not found after 120s"
    tail -20 "$LOGS_DIR/cardano-testnet.stderr"
    exit 1
fi
echo "Found node socket: $SOCKET_PATH"

# --- 3. Find node config ---
CONFIG_PATH=""
for name in configuration.yaml configuration.json config.json; do
    CONFIG_PATH=$(find "$TESTNET_DIR" -name "$name" 2>/dev/null | head -1 || true)
    [ -n "$CONFIG_PATH" ] && break
done
if [ -z "$CONFIG_PATH" ]; then
    echo "FAILED: node config not found"
    exit 1
fi
echo "Found node config: $CONFIG_PATH"

# --- 4. Start ogmios ---
echo ""
echo "--- Starting ogmios ---"
"$OGMIOS" \
  --node-socket "$SOCKET_PATH" \
  --node-config "$CONFIG_PATH" \
  --port "$OGMIOS_PORT" \
  --log-level error \
  > "$LOGS_DIR/ogmios.stdout" 2> "$LOGS_DIR/ogmios.stderr" &
OGMIOS_PID=$!
echo "ogmios PID: $OGMIOS_PID"

echo "Waiting for ogmios on port $OGMIOS_PORT..."
for i in $(seq 1 60); do
    if nc -z 127.0.0.1 "$OGMIOS_PORT" 2>/dev/null; then break; fi
    sleep 1
done
if ! nc -z 127.0.0.1 "$OGMIOS_PORT" 2>/dev/null; then
    echo "FAILED: ogmios not accepting connections after 60s"
    tail -20 "$LOGS_DIR/ogmios.stderr"
    exit 1
fi
echo "ogmios is ready on port $OGMIOS_PORT"

# --- 5. Create tx-generator config ---
echo ""
echo "--- Creating tx-generator config ---"
SIG_KEY=$(find "$TESTNET_DIR" -name "utxo.skey" -path "*/utxo1/*" 2>/dev/null | head -1 || true)
if [ -z "$SIG_KEY" ]; then
    echo "FAILED: signing key not found"
    exit 1
fi

CONFIG_FILE="$WORKDIR/tx-generator-config.json"
# submissionEndpointProtocol and submissionEndpointURI must be set together.
# The endpoint replaces targetNodes as the submission target: the compiler
# requires targetNodes to be empty alongside an endpoint, which is automatic
# here — --testnet-config-dir discovers no target nodes when an endpoint is
# configured.
cat > "$CONFIG_FILE" << EOF
{
  "tx_count": 10,
  "tps": 2,
  "inputs_per_tx": 2,
  "outputs_per_tx": 2,
  "tx_fee": 212345,
  "min_utxo_value": 1000000,
  "add_tx_size": 39,
  "init_cooldown": 5,
  "era": "Conway",
  "keepalive": 30,
  "debugMode": false,
  "plutus": null,
  "sigKey": "$SIG_KEY",
  "submissionEndpointProtocol": "Ogmios",
  "submissionEndpointURI": "ws://127.0.0.1:$OGMIOS_PORT"
}
EOF

# The benchmarking phase pays to the compiler's hardcoded "BenchmarkingDone"
# key; derive its address instead of hardcoding it. The cborHex must match
# keyBenchmarkDone in cardano-node's
# bench/tx-generator/src/Cardano/Benchmarking/Compiler.hs.
# (addr_test1vz4qz2ayucp7xvnthrx93uhha7e04gvxttpnuq4e6mx2n5gzfw23z @ magic 42)
cat > "$WORKDIR/benchmark-done.skey" << EOF
{
    "type": "PaymentSigningKeyShelley_ed25519",
    "description": "",
    "cborHex": "582016ca4f13fa17557e56a7d0dd3397d747db8e1e22fdb5b9df638abdb680650d50"
}
EOF
"$CARDANO_CLI" key verification-key \
  --signing-key-file "$WORKDIR/benchmark-done.skey" \
  --verification-key-file "$WORKDIR/benchmark-done.vkey"
BENCH_ADDR=$("$CARDANO_CLI" address build \
  --payment-verification-key-file "$WORKDIR/benchmark-done.vkey" \
  --testnet-magic "$TESTNET_MAGIC")

query_utxo_count() {
    "$CARDANO_CLI" query utxo \
      --address "$BENCH_ADDR" \
      --testnet-magic "$TESTNET_MAGIC" \
      --socket-path "$SOCKET_PATH" \
      --out-file /dev/stdout | jq 'length'
}

# --- 6. UTxO count before ---
echo ""
echo "--- UTxOs at benchmark address before tx-generator ---"
BEFORE=$(query_utxo_count)
echo "  $BEFORE UTxOs at $BENCH_ADDR"

# --- 7. Run tx-generator via Ogmios ---
echo ""
echo "--- Running tx-generator with Ogmios submission ---"
# the if/else keeps a failure from tripping `set -e` before the log tails
# and the exit-code report below get a chance to run
if ( cd "$WORKDIR" && "$TX_GENERATOR" json_highlevel "$CONFIG_FILE" \
  --testnet-config-dir "$TESTNET_DIR" \
  > "$LOGS_DIR/tx-generator.stdout" 2> "$LOGS_DIR/tx-generator.stderr" ); then
    TX_EXIT=0
else
    TX_EXIT=$?
fi

echo ""
echo "--- tx-generator stdout (last 30 lines) ---"
tail -30 "$LOGS_DIR/tx-generator.stdout"
echo ""
echo "--- tx-generator stderr (last 20 lines) ---"
tail -20 "$LOGS_DIR/tx-generator.stderr"

if [ "$TX_EXIT" -ne 0 ]; then
    echo ""
    echo "=== FAILED: tx-generator exit code $TX_EXIT ==="
    exit "$TX_EXIT"
fi

# --- 8. Wait for txs to land in blocks, then count UTxOs ---
echo ""
echo "--- Waiting for transactions to be included in blocks ---"
AFTER="$BEFORE"
for i in $(seq 1 60); do
    AFTER=$(query_utxo_count)
    echo "  [$i] $AFTER UTxOs at benchmark address"
    if [ "$AFTER" -gt "$BEFORE" ]; then
        sleep 5
        AFTER=$(query_utxo_count)
        echo "  [final] $AFTER UTxOs at benchmark address"
        break
    fi
    sleep 2
done

echo ""
echo "=== Results ==="
echo "  UTxOs before: $BEFORE"
echo "  UTxOs after:  $AFTER"
echo "  New UTxOs:    $((AFTER - BEFORE))"

# submissions were all accepted (or we exited above), so new UTxOs must
# have appeared by now for the run to count as a pass
if [ "$AFTER" -le "$BEFORE" ]; then
    echo ""
    echo "=== FAILED: no new UTxOs appeared at the benchmark address ==="
    exit 1
fi

echo ""
echo "=== PASSED ==="
exit 0
