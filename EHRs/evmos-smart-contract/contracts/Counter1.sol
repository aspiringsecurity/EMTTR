// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Counter1 {
    uint256 public count = 1;

    function increment() public {
        count += 1;
    }
}