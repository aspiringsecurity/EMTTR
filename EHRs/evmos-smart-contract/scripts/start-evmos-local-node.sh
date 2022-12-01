#!/bin/bash

SCRIPT=$(basename ${BASH_SOURCE[0]})
KEY="mykey"
CHAINID="evmos_9000-1"
MONIKER="mymoniker"
ENABLE_CACHE="true"

#---------------------#

echo "Cleaning up ..."
pkill evmosd
rm -rf /tmp/evmos-*

#----------#

DATA_DIR=$(mktemp -d -t evmos-datadir.XXXXX)
if [[ ! "$DATA_DIR" ]]; then
    echo "Could not create $DATA_DIR"
    exit 1
fi
echo -e "DATA_DIR=${DATA_DIR}"


REPO_DIR=$(mktemp -d -t evmos-repo.XXXXX)
if [[ ! "${REPO_DIR}" ]]; then
    echo "Could not create ${REPO_DIR}"
    exit 1
fi

#----------#

set -euo pipefail

init_func() {
    
    EVMOSD_PATH=$(which evmosd)
    cd ${REPO_DIR}
    if [[ ${ENABLE_CACHE} == "true" ]] && [[ ${EVMOSD_PATH} != "" ]]; then
        echo -e "Copying the installed evmosd binary from ${EVMOSD_PATH} to ${PWD}/build/evmosd ..."
        mkdir -p "${PWD}"/build/
        cp ${EVMOSD_PATH} "${PWD}"/build/
    else
        # clone and compile evmos
        echo "Cloning and compiling evmos ..."
        git clone https://github.com/evmos/evmos.git
        cd evmos
        make build
    fi

    echo -e "BIN_PATH=${PWD}/build/evmosd"

    "${PWD}"/build/evmosd keys add $KEY --keyring-backend test --home "${DATA_DIR}" --no-backup --algo "eth_secp256k1"
    "${PWD}"/build/evmosd init $MONIKER --chain-id $CHAINID --home "${DATA_DIR}"

    # Change parameter token denominations to aevmos
    cat ${DATA_DIR}/config/genesis.json | jq '.app_state["staking"]["params"]["bond_denom"]="aevmos"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json
    cat ${DATA_DIR}/config/genesis.json | jq '.app_state["crisis"]["constant_fee"]["denom"]="aevmos"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json
    cat ${DATA_DIR}/config/genesis.json | jq '.app_state["gov"]["deposit_params"]["min_deposit"][0]["denom"]="aevmos"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json
    cat ${DATA_DIR}/config/genesis.json | jq '.app_state["evm"]["params"]["evm_denom"]="aevmos"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json
    cat ${DATA_DIR}/config/genesis.json | jq '.app_state["inflation"]["params"]["mint_denom"]="aevmos"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json

    # Set gas limit in genesis
    cat ${DATA_DIR}/config/genesis.json | jq '.consensus_params["block"]["max_gas"]="10000000"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json

    # Set claims start time
    node_address=$("${PWD}"/build/evmosd keys list --home "${DATA_DIR}" --keyring-backend test | grep  "address: " | cut -c12-)
    current_date=$(date -u +"%Y-%m-%dT%TZ")
    cat ${DATA_DIR}/config/genesis.json | jq -r --arg current_date "$current_date" '.app_state["claims"]["params"]["airdrop_start_time"]=$current_date' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json

    # Set claims records for validator account
    amount_to_claim=10000
    cat ${DATA_DIR}/config/genesis.json | jq -r --arg node_address "$node_address" --arg amount_to_claim "$amount_to_claim" '.app_state["claims"]["claims_records"]=[{"initial_claimable_amount":$amount_to_claim, "actions_completed":[false, false, false, false],"address":$node_address}]' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json

    # Set claims decay
    cat ${DATA_DIR}/config/genesis.json | jq -r --arg current_date "$current_date" '.app_state["claims"]["params"]["duration_of_decay"]="1000000s"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json
    cat ${DATA_DIR}/config/genesis.json | jq -r --arg current_date "$current_date" '.app_state["claims"]["params"]["duration_until_decay"]="100000s"' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json

    # Claim module account:
    # 0xA61808Fe40fEb8B3433778BBC2ecECCAA47c8c47 || evmos15cvq3ljql6utxseh0zau9m8ve2j8erz89m5wkz
    cat ${DATA_DIR}/config/genesis.json | jq -r --arg amount_to_claim "$amount_to_claim" ".app_state[\"bank\"][\"balances\"] += [{\"address\":\"evmos15cvq3ljql6utxseh0zau9m8ve2j8erz89m5wkz\",\"coins\":[{\"denom\":\"aevmos\", \"amount\":\"$amount_to_claim\"}]}]" > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json

    "${PWD}"/build/evmosd add-genesis-account \
    "$("${PWD}"/build/evmosd keys show "$KEY" --keyring-backend test -a --home "$DATA_DIR")" 100000000000000000000000000aevmos,1000000000000000000stake \
    --keyring-backend test --home "${DATA_DIR}"

    validators_supply=$(cat ${DATA_DIR}/config/genesis.json | jq -r '.app_state["bank"]["supply"][0]["amount"]')
    # Bc is required to add this big numbers
    # total_supply=$(bc <<< "$amount_to_claim+$validators_supply")
    total_supply=100000000000000000000010000
    cat ${DATA_DIR}/config/genesis.json | jq -r --arg total_supply "$total_supply" '.app_state["bank"]["supply"][0]["amount"]=$total_supply' > ${DATA_DIR}/config/tmp_genesis.json && mv ${DATA_DIR}/config/tmp_genesis.json ${DATA_DIR}/config/genesis.json

    "${PWD}"/build/evmosd gentx "$KEY" 1000000000000000000000aevmos --chain-id ${CHAINID} --keyring-backend test --home "${DATA_DIR}"

    "${PWD}"/build/evmosd collect-gentxs --home "${DATA_DIR}"
    "${PWD}"/build/evmosd validate-genesis --home "${DATA_DIR}"
}

start_func() {
    echo "starting evmos node  in background ..."
    "${PWD}"/build/evmosd start --pruning=nothing --rpc.unsafe \
    --keyring-backend test --home "$DATA_DIR" \
    >"$DATA_DIR"/node.log 2>&1 & disown

    # --p2p.laddr tcp://${IP_ADDR}:${NODE_P2P_PORT} --address tcp://${IP_ADDR}:${NODE_PORT} --rpc.laddr tcp://${IP_ADDR}:${NODE_RPC_PORT} \
    # --json-rpc.address=${IP_ADDR}:${RPC_PORT} \

    EVMOS_PID=$!
    echo "started evmos node, pid=${EVMOS_PID}"
}

# Since tx s fail due to this error: max fee per gas less than block base fee (1000000 < 1105735)
# and waiting to reach to x blockheight seems to resovle it, let's keep waiting until then
wait_func() {
    echo "waiting for evmos to get ready..."
    while true; do
        LATEST_BLOCK_HEIGHT=$(curl -s http://localhost:26657/status 2>&1 | jq -r ".result.sync_info.latest_block_height")
        printf "\rHeight: %s" "$LATEST_BLOCK_HEIGHT"
        if ((LATEST_BLOCK_HEIGHT>50)); then
            echo "evmos node started"
            break
        fi
        sleep 1
    done
}

#---- Main ----#


init_func
start_func
sleep 2
wait_func

echo ${DATA_DIR} > "/tmp/evmos-test-data-dir"
echo "Data dir path is written into /tmp/evmos-test-data-dir :"
cat /tmp/evmos-test-data-dir