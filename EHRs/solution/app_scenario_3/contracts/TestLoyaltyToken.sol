/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./LoyaltyToken.sol";

/* Test only repeats correctly if msg.sender is not changed */
contract TestLoyaltyToken is LoyaltyToken {
    address receiver = 0xf5b475Fc347C4BA87E863f0b5269db08Af18f248;

    function testInitiateToken() public {
        isTokenInitialized = false;
        balanceOf[receiver] = 0;

        initiateToken(10000 ether, 500 ether, "LoyalToken", "LT", 18);
        
        assert(balanceOf[msg.sender] == 9500 ether);
        assert(keccak256(bytes(symbol)) == keccak256("LT"));
        assert(isTokenInitialized);
    }

    function testTransfer() public {
        testInitiateToken();
        
        transfer(receiver, 100 ether);
        
        assert(balanceOf[msg.sender] == 9400 ether);
        assert(balanceOf[receiver] == 100 ether);
    }

    function testApprove() public {
        testInitiateToken();
 
        approve(msg.sender, 200 ether);
        
        assert(allowance[msg.sender][msg.sender] == 200 ether);
    }
    
    function testTransferFrom() public {
        testApprove();
        
        transferFrom(msg.sender, receiver, 100 ether);
        
        assert(balanceOf[msg.sender] == 9400 ether);
        assert(balanceOf[receiver] == 100 ether);
        assert(allowance[msg.sender][msg.sender] == 100 ether);
    }
    
    function testTransferBonus() public {
        testInitiateToken();
        
        transferBonus(receiver, 500 ether);
        
        assert(balanceOf[receiver] == 500 ether);
    }
}