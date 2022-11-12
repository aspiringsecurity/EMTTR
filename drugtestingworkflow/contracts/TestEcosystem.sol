pragma solidity ^0.4.24;

import "./Ecosystem.sol";

contract TestEcosystem is Ecosystem {

    address taskOwner1 = 0x39D5aDEceAB09F641971589897506663cE2e52B6;
    address taskOwner2 = 0x5bf917bF2b0a3C51b95582fc72Cb241Aa43F34BA;
    address taskOwner3 = 0x9F3CB330186FF9D098f5A8CD721a8DDA7289dBC7;
    address aiPlayer1 = 0xe6b8C499aC36D23A793a63E3b83AC3Cb2Fd2b8AA;
    address aiPlayer2 = 0xB0dCcBF51fb6fc97371B4c1610AF8f0001B8fE23;
    address aiPlayer3 = 0xA58889E4B99230E14EFC30Cc5615EC88AD987821;

    function testInitializeToken() public {
        initiateToken("HEATH","HBAR",16,1000000000);
    }

    function testPrepUser() public {
        testInitializeToken();
        // fund task owner 1 HBAR
        transfer(taskOwner1, 1000000000000000000);
        transfer(taskOwner2, 1000000000000000000);
        transfer(taskOwner3, 1000000000000000000);
        // fund task owner 0.2 HBAR
        transfer(aiPlayer1, 200000000000000000);
        transfer(aiPlayer2, 200000000000000000);
        transfer(aiPlayer3, 200000000000000000);
        assert(balanceOf(taskOwner1) == 1000000000000000000);
        assert(balanceOf(taskOwner2) == 1000000000000000000);
        assert(balanceOf(taskOwner3) == 1000000000000000000);
        assert(balanceOf(aiPlayer1) == 200000000000000000);
        assert(balanceOf(aiPlayer2) == 200000000000000000);
        assert(balanceOf(aiPlayer3) == 200000000000000000);
    }
    // One drug launch team initiates a drug task (requires only one AI team to compute drug efficacy) 
    // and add to the ecosystem. An AI team picks the task, compute drug efficacy, and return the result.
    // Then the drug team confirms the result. Finally, the drug R&D ecosystem award the AI team with
    // loyalty tokens
    function testHandshake101() public {
        testPrepUser();
        string memory t = "AI Compute";
        string memory url = "https://drive.google.com/open?id=12345_abcdefg-HIJK";
        string memory h = "c8e1f08945eb55208947cb1c76d911ed";
        uint256 nextTaskCount = numTask()+1;
        createTask(taskOwner1, t , 100000000000000000 , url , h);
        assert(numTask() == nextTaskCount);
        // approving
        approveTask(nextTaskCount);
        // submit one result
        uint256 nextResultCount = numResult()+1;
        submitResult(aiPlayer1, nextTaskCount, 10, url , h);
        assert(numResult() == nextResultCount);
        // confirm the result
        approveResult(nextResultCount);
        // award
        uint256 before = balanceOf(aiPlayer1);
        award(nextTaskCount);

        assert(tasks[nextTaskCount].status == 2);
        before = before + 100000000000000000;
        assert(balanceOf(aiPlayer1) == before);
    }

    // One drug launch team initiates a drug task (requires three AI team to compute drug efficacy) and
    // add to the ecosystem. Three AI teams pick the task, compute drug efficacy, and return their results.
    // Then the drug team confirms the results. Finally, the drug R&D ecosystem do cross validation, 
    // and award the AI team having best result with loyalty tokens.
    function testChooseTheBest() public {
        testPrepUser();
        string memory t = "AI Compute";
        string memory url = "https://drive.google.com/open?id=12345_abcdefg-HIJK";
        string memory h = "c8e1f08945eb55208947cb1c76d911ed";

        uint256 nextTaskCount = numTask()+1;
        createTask(taskOwner1, t , 100000000000000000 , url , h);
        assert(numTask() == nextTaskCount);

        approveTask(nextTaskCount);

        uint256 nextResultCount = numResult()+3;
        submitResult(aiPlayer1, nextTaskCount, 10, url , h);
        approveResult(nextResultCount-2);
        submitResult(aiPlayer2, nextTaskCount, 20, url , h);
        approveResult(nextResultCount-1);
        submitResult(aiPlayer3, nextTaskCount, 30, url , h);
        approveResult(nextResultCount);
        assert(numResult() == nextResultCount);
        
        uint256 before1 = balanceOf(aiPlayer1);
        uint256 before2 = balanceOf(aiPlayer2);
        uint256 before3 = balanceOf(aiPlayer3);
        award(nextTaskCount);
        before3 = before3 + 100000000000000000;
        assert(balanceOf(aiPlayer1) == before1);
        assert(balanceOf(aiPlayer2) == before2);
        assert(balanceOf(aiPlayer3) == before3);
        assert(tasks[nextTaskCount].status == 2);
        
    }

    // Many (at least three) drug launch teams initiate many (at least five) drug tasks in total. These five
    // tasks are added to a pool. Three AI teams pick tasks from the pool individually. Then for each 
    // finished task, the drug R&D ecosystem do cross validation, and award the best AI team with 
    // loyalty tokens. 
    function testCompeteForSuccess() public {
        testPrepUser();
        string memory t = "AI Compute";
        string memory url = "https://drive.google.com/open?id=12345_abcdefg-HIJK";
        string memory h = "c8e1f08945eb55208947cb1c76d911ed";

        uint256 nextTaskCount = numTask()+5;
        createTask(taskOwner1, t , 50000000000000000 , url , h);
        approveTask(nextTaskCount-4);
        createTask(taskOwner1, t , 50000000000000000 , url , h);
        approveTask(nextTaskCount-3);
        createTask(taskOwner2, t ,100000000000000000 , url , h);
        approveTask(nextTaskCount-2);
        createTask(taskOwner2, t , 100000000000000000 , url , h);
        approveTask(nextTaskCount-1);
        createTask(taskOwner3, t , 150000000000000000 , url , h);
        approveTask(nextTaskCount);
        assert(numTask() == nextTaskCount);
        
        uint256 max = 5;
        for (uint256 i=0; i<max;i++) {
            //result = result-10;
            int resultAi1 = 0;
            int resultAi2 = 0;
            int resultAi3 = 0;
            if (i==0) {
                resultAi1 = 10;
                resultAi2 = 20;
                resultAi3 = 30;
            }  else if (i==1) {
                resultAi1 = 25;
                resultAi2 = 50;
                resultAi3 = 35;
            } else if (i==2) {
                resultAi1 = 50;
                resultAi2 = 40;
                resultAi3 = 30;
            } else if (i==3) {
                resultAi1 = 12;
                resultAi2 = 13;
                resultAi3 = 14;
            } else if (i==4) {
                resultAi1 = 77;
                resultAi2 = 54;
                resultAi3 = 98;
            } 
            uint256 nextResultCount = numResult()+3;
            
            submitResult(aiPlayer1, nextTaskCount-4+i, resultAi1, url , h);
            approveResult(nextResultCount-2);
            submitResult(aiPlayer2, nextTaskCount-4+i, resultAi2, url , h);
            approveResult(nextResultCount-1);
            submitResult(aiPlayer3, nextTaskCount-4+i, resultAi3, url , h);
            approveResult(nextResultCount);
        }

        uint256 before1 = balanceOf(aiPlayer1);
        uint256 before2 = balanceOf(aiPlayer2);
        uint256 before3 = balanceOf(aiPlayer3);

        award(nextTaskCount-4);
        award(nextTaskCount-3);
        award(nextTaskCount-2);
        award(nextTaskCount-1);
        award(nextTaskCount);
        before1 = before1 + 100000000000000000;
        before2 = before2 + 50000000000000000;
        before3 = before3 + 300000000000000000;
        assert(balanceOf(aiPlayer1) == before1);
        assert(balanceOf(aiPlayer2) == before2);
        assert(balanceOf(aiPlayer3) == before3);
        
        

        
    }

}