// SPDX-License-Identifier: MIT
pragma solidity ^0.8.7;

import {Functions, FunctionsClient} from "./dev/functions/FunctionsClient.sol";
// import "@chainlink/contracts/src/v0.8/dev/functions/FunctionsClient.sol"; // Once published
import {ConfirmedOwner} from "@chainlink/contracts/src/v0.8/ConfirmedOwner.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721URIStorage.sol";
import "@openzeppelin/contracts/utils/Strings.sol";
import "@openzeppelin/contracts/utils/Base64.sol";
import "@openzeppelin/contracts/utils/Counters.sol";

/**
 * @title Functions Consumer contract
 * @notice This contract is a demonstration of using Functions.
 * @notice NOT FOR PRODUCTION USE
 */
contract FunctionsConsumer is FunctionsClient, ConfirmedOwner, ERC721URIStorage {
  using Functions for Functions.Request;
  using Counters for Counters.Counter;
  
  Counters.Counter private _tokenIdCounter;
  bytes32 public latestRequestId;
  bytes public latestResponse;
  bytes public latestError;
  uint256 public SxTId;

  event OCRResponse(bytes32 indexed requestId, bytes result, bytes err);
  event SxTNFT(string name, uint256 id);
  event BatchMetadataUpdate(uint256 _fromTokenId, uint256 _toTokenId);
  /**
   * @notice Executes once when a contract is created to initialize state variables
   *
   * @param oracle - The FunctionsOracle contract
   */
  // https://github.com/protofire/solhint/issues/242
  // solhint-disable-next-line no-empty-blocks
  // constructor(address oracle) FunctionsClient(oracle) ConfirmedOwner(msg.sender) {}
  constructor(address oracle) FunctionsClient(oracle) ConfirmedOwner(msg.sender) ERC721("Space & Time dNFT", "SXT-DNFT") {
    _safeMint(msg.sender, 0);
  }

  /**
   * @notice Send a simple request
   *
   * @param source JavaScript source code
   * @param secrets Encrypted secrets payload
   * @param args List of arguments accessible from within the source code
   * @param subscriptionId Funtions billing subscription ID
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
    if (secrets.length > 0) {
      req.addRemoteSecrets(secrets);
    }
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
    SxTId = abi.decode(response, (uint256));
    emit BatchMetadataUpdate(0, type(uint256).max);
  }
  
  // MINT YO
  function mintNFT(address to) public onlyOwner {
    _tokenIdCounter.increment();
    _safeMint(to, _tokenIdCounter.current());
  }

// HERE WE ARE  
  function tokenURI(uint256) public view override(ERC721URIStorage) returns (string memory) {
    string memory baseURL = "https://cloudflare-ipfs.com/ipfs/QmYxCeAjwBiAHUztrFGt3e4ZZEV8txJdYSzVdk6YTWn84j/";
    return  string(abi.encodePacked(baseURL, string(Strings.toString(SxTId))));
  }



 // The following function is an override required by Solidity.
  function _burn(uint256 tokenId) internal override(ERC721URIStorage) {
    super._burn(tokenId);
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
