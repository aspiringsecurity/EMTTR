#!/usr/bin/python3
from brownie import APIConsumer, config, network
from web3 import Web3
from scripts.helpful_scripts import (
    BLOCK_CONFIRMATIONS_FOR_VERIFICATION,
    get_account,
    get_contract,
    is_verifiable_contract,
)


def deploy_api_consumer():
    jobId = config["networks"][network.show_active()]["jobId"]
    fee = config["networks"][network.show_active()]["fee"]
    account = get_account()
    oracle = get_contract("oracle").address
    link_token = get_contract("link_token").address
    api_consumer = APIConsumer.deploy(
        oracle,
        Web3.toHex(text=jobId),
        fee,
        link_token,
        {"from": account},
    )

    if is_verifiable_contract():
        api_consumer.tx.wait(BLOCK_CONFIRMATIONS_FOR_VERIFICATION)
        APIConsumer.publish_source(api_consumer)

    print(f"API Consumer deployed to {api_consumer.address}")
    return api_consumer


def main():
    deploy_api_consumer()
