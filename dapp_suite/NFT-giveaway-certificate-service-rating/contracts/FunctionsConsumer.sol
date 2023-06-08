// SPDX-License-Identifier: MIT
pragma solidity ^0.8.7;

import {Functions, FunctionsClient} from "./dev/functions/FunctionsClient.sol";
// import "@chainlink/contracts/src/v0.8/dev/functions/FunctionsClient.sol"; // Once published
import {ConfirmedOwner} from "@chainlink/contracts/src/v0.8/ConfirmedOwner.sol";
import {GiveawayNFT} from "./GiveawayNFT.sol";

/**
 * @title Functions Consumer contract
 * @notice This contract is a demonstration of using Functions.
 * @notice NOT FOR PRODUCTION USE
 */
contract FunctionsConsumer is FunctionsClient, ConfirmedOwner {
  using Functions for Functions.Request;

  bytes32 public latestRequestId;
  bytes public latestResponse;
  bytes public latestError;

  GiveawayNFT public nft;

  event OCRResponse(bytes32 indexed requestId, bytes result, bytes err);

  /**
   * @notice Executes once when a contract is created to initialize state variables
   *
   * @param oracle - The FunctionsOracle contract
   */
  constructor(address oracle) FunctionsClient(oracle) ConfirmedOwner(msg.sender) {
    nft = new GiveawayNFT();
  }

  /**
   * @notice Send a simple request
   *
   * @param source JavaScript source code
   * @param secrets Encrypted secrets payload
   * @param args List of arguments accessible from within the source code
   * @param subscriptionId Billing ID
   * @param gasLimit Maximum amount of gas used to call the client contract's `handleOracleFulfillment` function
   * @return Functions request ID
   */
  function executeRequest(
    string calldata source,
    bytes calldata secrets,
    string[] calldata args,
    uint64 subscriptionId,
    uint32 gasLimit
  ) public onlyOwner returns (bytes32) {
    Functions.Request memory req;
    req.initializeRequest(Functions.Location.Inline, Functions.CodeLanguage.JavaScript, source);
    if (secrets.length > 0) req.addRemoteSecrets(secrets);
    if (args.length > 0) req.addArgs(args);

    bytes32 assignedReqID = sendRequest(req, subscriptionId, gasLimit);
    latestRequestId = assignedReqID;
    return assignedReqID;
  }

  /**
   * @notice Callback that is invoked once the DON has resolved the request or hit an error
   *
   * @param requestId The request ID, returned by sendRequest()
   * @param response Aggregated response from the user code
   * @param err Aggregated error from the user code or from the execution pipeline
   * Either response or error parameter will be set, but never both
   */
  function fulfillRequest(bytes32 requestId, bytes memory response, bytes memory err) internal override {
    latestResponse = response;
    latestError = err;
    emit OCRResponse(requestId, response, err);

    address winner1;
    address winner2;
    address winner3;

    assembly {
      winner1 := mload(add(response, 20))
      winner2 := mload(add(response, 40))
      winner3 := mload(add(response, 60))
    }

    nft.mint(winner1);
    nft.mint(winner2);
    nft.mint(winner3);
  }

  /**
   * @notice Allows the Functions oracle address to be updated
   *
   * @param oracle New oracle address
   */
  function updateOracleAddress(address oracle) public onlyOwner {
    setOracle(oracle);
  }

  function addSimulatedRequestId(address oracleAddress, bytes32 requestId) public onlyOwner {
    addExternalRequest(oracleAddress, requestId);
  }
}
