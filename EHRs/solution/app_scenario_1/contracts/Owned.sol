pragma solidity ^0.4.24;

contract Owned {

    address public owner;

    constructor () public {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        require(msg.sender == owner, "Not owner");
        _;
    }

    function setOwner(address _newOwner) onlyOwner public {
        owner = _newOwner;
    }
}
