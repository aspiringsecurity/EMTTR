//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

contract Greeter {
    string private greeting;

    constructor() {
        greeting = "Hello";
    }

    function greet() public view returns (string memory) {
        return greeting;
    }

    function setGreeting(string memory _greeting) public payable {
        greeting = _greeting;
    }

    function retrieve() public {
        if (address(this).balance > 0) {
            (bool sent, ) = payable(msg.sender).call{
                value: address(this).balance
            }("");
            require(sent, "Failed to send Ether");
        }
    }
}
