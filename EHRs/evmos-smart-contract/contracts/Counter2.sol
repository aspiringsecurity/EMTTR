// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Counter2 {
    uint256 public count = 1;
    string public message;

    constructor(uint256 initValue, string memory initTxt) {

      // Accepts a string argument `initMessage` and sets the value into the contract's `message` storage variable).
      count = initValue;
      message = initTxt;
   }

    function increment() public {
        count += 1;
    }
}