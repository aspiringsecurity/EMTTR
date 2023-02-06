//SPDX-License-Identifier: MIT
pragma solidity ^0.8.9;

import "./ITuringHelper.sol";
import "@openzeppelin/contracts/utils/Strings.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "base64-sol/base64.sol";

contract Calculator is Ownable {

    ITuringHelper public hcHelper;
    string private _hcEndpoint;

    event CalcResult(uint256 result);

    constructor(address hcHelper_, string memory hcEndpoint_) {
        _hcEndpoint = hcEndpoint_;
        hcHelper = ITuringHelper(hcHelper_);
    }

    function setTuringHelper(address hcHelper_) external onlyOwner {
        hcHelper = ITuringHelper(hcHelper_);
    }

    function setEndpoint(string memory hcEndpoint_) external onlyOwner {
        _hcEndpoint = hcEndpoint_;
    }

    // TODO: Add your HybridCompute function here
    /** @dev The backend expects two integers: properTime, velocity.
      * API call returns integer with result. */
    function calcTimeDilation(uint256 properTime, uint256 velocity) external {
        // 1. Encode parameters
        // 2. Send request and get bytes
        // 3. Decode bytes to expected dataTypes

        emit CalcResult(res);
    }


}
