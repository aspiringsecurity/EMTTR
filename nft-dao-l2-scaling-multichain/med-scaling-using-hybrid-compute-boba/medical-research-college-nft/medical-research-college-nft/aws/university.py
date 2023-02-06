# https://github.com/bobanetwork/boba/blob/develop/packages/boba/turing/AWS_code/turing_oracle.py

import json
import urllib3
import certifi
import textwrap
from eth_abi import encode_abi, decode_abi
from web3 import Web3

authorized_contract = None  # for open access


# TODO: Maybe create production script for this, simply for readability and to waste less computation ms on Lambda

# NOTE: When taking the data payload from the original event for debugging then remove the first 64 bits!
def lambda_handler(event, context):
    print_logs = True
    show_new_degree = False  # only used in test-env (= when print_logs is False)

    if "logs" in event:
        print_logs = event["logs"]

    if not print_logs and "showNewDegree" in event:
        show_new_degree = event["showNewDegree"]

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
    # str_length_1 = int(params[1], 16) * 2

    # the message sender
    sender_address = params[0]
    sender_address = sender_address[-40:64]

    if print_logs:
        print("Wallet: ", sender_address)

    res = load_data(sender_address, print_logs, show_new_degree)

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


def load_data(senderAddress, print_logs=True, show_new_degree=False):
    # Create a PoolManager instance for sending requests.
    http = urllib3.PoolManager(ca_certs=certifi.where())

    # TODO: Send a POST request and receive a HTTPResponse object.
    # error_reason = 0
    # headers = {'Authorization': 'Bearer ' + TWITTER_BEARER_TOKEN}
    # resp = http.request("GET",
    #                    "https://api.twitter.com/2/tweets/" + student_id + "?expansions=author_id&user.fields=created_at,public_metrics",
    #                    headers=headers)
    # result = json.loads(resp.data)
    # print("result: ", result)

    # create return payload
    majors = ["Computer Science 2022", "History 1994"]
    linkedin_username = "wsdt"

    if show_new_degree:
        majors.append("Bio-Engineering 1986")

    encoded_str = encode_abi(['string[]', 'string'], [majors, linkedin_username])
    res = Web3.toHex(encoded_str)

    if print_logs:
        decoded = decode_abi(['string[]', 'string'], encoded_str)
        print("ENCODED: ", encoded_str)
        print("DECODED: ", decoded)
        print("RES: ", res)

    return res
