// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import "forge-std/Script.sol";
import "../src/HelloWorld.sol";

contract HelloWorldScript is Script {
    function setUp() public {}

    function run() public {
        vm.broadcast();
        HelloWorld test = new HelloWorld();
    }
}
