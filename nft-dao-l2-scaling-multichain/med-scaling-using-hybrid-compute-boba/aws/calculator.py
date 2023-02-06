# https://github.com/bobanetwork/boba/blob/develop/packages/boba/turing/AWS_code/turing_oracle.py

import json
import math
import textwrap
from eth_abi import encode_abi, decode_abi
from web3 import Web3

authorized_contract = None  # for open access


# NOTE: When taking the data payload from the original event for debugging then remove the first 64 bits!
def lambda_handler(event, context):
    print_logs = True

    if "logs" in event:
        print_logs = event["logs"]

    if print_logs:
        print("Lambda DEBUG: ", event)

    input = json.loads(event["body"])
    if print_logs:
        print("Lambda DEBUG: from Geth:", input)

    if authorized_contract is not None:
        # check authorisation if desired
        callerAddress = input['method']
        if callerAddress.lower() != authorized_contract.lower():
            return_payload = {'statusCode': 403}
            if print_logs:
                print('return payload:', return_payload)
            return return_payload

    # get calling parameters
    paramsHexString = input['params'][0]
    paramsHexString = paramsHexString.removeprefix("0x")
    params = textwrap.wrap(paramsHexString, 64)

    if print_logs:
        print("Params: ", params)

    # the message sender
    # [0,1] for V1, [1,2] for legacy
    proper_time = int(params[1], 16)
    velocity = int(params[2], 16)

    if print_logs:
        print("Params: ", proper_time, velocity)

    res = calc_result(proper_time, velocity, print_logs)

    # example res:
    # 0x
    # 0000000000000000000000000000000000000000000000000000000000000040
    # 0000000000000000000000000000000000000000000000000000000000418b95
    # 0000000000000000000000000000000000000000000000000000017e60d3b45f

    return_payload = {
        "statusCode": 200,
        "body": json.dumps({
            "result": res
        })
    }

    if print_logs:
        print('return payload:', return_payload)

    return return_payload


SPEED_OF_LIGHT = 299792.458  # in km/s


def calc_result(proper_time, velocity, print_logs=True):
    result = proper_time / (math.sqrt(1 - math.pow((velocity / SPEED_OF_LIGHT), 2)))
    result *= 1_000_000_000_000 # to make it usable as uint

    # V1 no length param (first value), for legacy first param length needed
    encoded_str = encode_abi(['uint256', 'uint256'], [32, int(result)])
    res = Web3.toHex(encoded_str)

    if print_logs:
        decoded = decode_abi(['uint256', 'uint256'], encoded_str)
        print("ENCODED: ", encoded_str)
        print("DECODED: ", decoded)
        print("RES: ", res)

    return res
