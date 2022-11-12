pragma solidity ^0.4.24;

import "./Token.sol";

contract TestToken is Token {
    
    function testInitializeToken() public {
        initiateToken("HEATH","HBAR",16,1000000000);
        assert(balances[msg.sender] == 1000000000);
    }

}