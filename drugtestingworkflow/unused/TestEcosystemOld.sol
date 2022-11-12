pragma solidity ^0.4.24;

import "./Ecosystem.sol";

contract TestEcosystem is Ecosystem {

    address taskOwner = 0x6f1367FE0f83f00E398091ba86AF53565bf4a786;
    address player1 = 0x39D5aDEceAB09F641971589897506663cE2e52B6;
    address player2 = 0x5bf917bF2b0a3C51b95582fc72Cb241Aa43F34BA;


    function testDeployLoyaltyToken() public {
        initToken("HEATH","HBAR",16,1000000000);
        assert(balanceOf(msg.sender) == 1000000000);
    }

    function testTransferToUser() public {
        testDeployLoyaltyToken();
        transfer(taskOwner,100000);
        transfer(player1,5000);
        transfer(player2,5000);
        assert(balanceOf(taskOwner) == 100000);
        assert(balanceOf(player1) == 5000);
        assert(balanceOf(player2) == 5000);
    }

    function testCreateOneTaskAndAward() public {
        testTransferToUser();
        createTask(0x6f1367FE0f83f00E398091ba86AF53565bf4a786,"task1",10000,"url","hash",2);
        assert(balanceOf(taskOwner) == 90000);
        submitResult(0x39D5aDEceAB09F641971589897506663cE2e52B6,1,2,"url1","my_hash1");
        submitResult(0x5bf917bF2b0a3C51b95582fc72Cb241Aa43F34BA,1,1,"url2","my_hash2");
        assert(numTask() == 1);
        assert(numResult() == 2);
        awardFirstPlayer(1);
        assert(balanceOf(player1) == 15000);
        assert(numTask() == 0);
    }

    
}