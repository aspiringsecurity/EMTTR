/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
pragma solidity ^0.4.24;

import "./DrugTask.sol";

contract TestDrugTask is DrugTask {
    /* Notice:
     * Due to contract size limit of 2KB (see EIP-170),
     * only part of encryptCode and dataHash values are used in the unit test */

    function testInitiateTaskStatus(uint8 status) private {
        require(status >= 0 && status < 4);
        address playerAddr = 0xf5b475Fc347C4BA87E863f0b5269db08Af18f248;
        /* Initiate Task */
        task.taskName = "AI Compute";
        task.toolName = "ADMET";
        task.numPlayers = 0;
        task.maxPlayers = 2;
        task.playerSalary = 0.05 ether;
        task.isFinished = false;
        task.timeStamp = 1537142012;
        isTaskInitialized = true;

        if (status == 0 || status == 2) {
            owner = playerAddr;
            playerAddr = msg.sender;
        } else {
            owner = msg.sender;
        }
        
        if (status > 0) {
            string memory encryptCode_0 = "03c57256ec2faf84828287710dfea24e";
            // Clean playerPool and dataReqPool
            playerPool.length = 0;
            dataReqPool.length = 0;
            // Initiate player pool
            playerPool.push(Player(playerAddr, 0, 0, 0, 1, 1537142345));
            // Initiate data request pool
            dataReqPool.push(Request(playerAddr, encryptCode_0, "", "", false, 1537142345, owner));
        }

        if (status > 1) {
            string memory dataURL_0 = "https://drive.google.com/open?id=12345_abcdefg-HIJK";
            string memory dataHash_0 = "c8e1f08945eb55208947cb1c76d911ed";
            string memory encryptCode_1 = "02b42006fcdf443951edf262081a3";
            // Initiate player pool
            playerPool[0].dataOutputId = 1;
            playerPool[0].status = 2;
            task.numPlayers = 1;
            // Initiate data request pool
            dataReqPool[0].dataURL = dataURL_0;
            dataReqPool[0].dataHash = dataHash_0;
            dataReqPool[0].isApproved = true;
            dataReqPool.push(Request(owner, encryptCode_1, "", "", false, 1537142480, playerAddr));
        }
   
        if (status > 2) {
            string memory dataURL_1 = "https://drive.google.com/open?id=67890_opqrst-UVWXYZ";
            string memory dataHash_1 = "2db120a041647ba660990e68624cd";
            // Initiate Token
            isTokenInitialized = false;
            balanceOf[playerAddr] = 0;
            initiateToken(10000 ether, 500 ether, "LoyalToken", "LT", 18);
            // Initiate player pool
            playerPool[0].result = 0.5 ether;
            playerPool[0].status = 3;
            //*
            // Initiate data request pool
            dataReqPool[1].dataURL = dataURL_1;
            dataReqPool[1].dataHash = dataHash_1;
            dataReqPool[1].isApproved = true;
            //*/
        }
    }
    
    function testSubmitTaskRequest() public {
        string memory encryptCode_0 = "03c57256ec2faf84828287710dfea24e";
        testInitiateTaskStatus(0);
        
        submitTaskRequest(owner, encryptCode_0);
    }
    
    function testApproveTaskRequest() public {
        string memory encryptCode_1 = "02b42006fcdf443951edf262081a3";
        string memory dataURL_0 = "https://drive.google.com/open?id=12345_abcdefg-HIJK";
        string memory dataHash_0 = "c8e1f08945eb55208947cb1c76d911ed";
        testInitiateTaskStatus(1);
        
        approveTaskRequest(0, encryptCode_1, dataURL_0, dataHash_0);

        assert(task.numPlayers == 1);
        assert(playerPool[0].dataOutputId == 1);
        assert(playerPool[0].status == 2);
        assert(dataReqPool[0].isApproved);
    }
    
    function testSubmitTaskResult() public {
        string memory dataURL_1 = "https://drive.google.com/open?id=67890_opqrst-UVWXYZ";
        string memory dataHash_1 = "2db120a041647ba660990e68624cd";
        testInitiateTaskStatus(2);
        
        submitTaskResult(0, dataURL_1, dataHash_1, 0.5 ether);

        assert(playerPool[0].result == 0.5 ether);
        assert(playerPool[0].status == 3);
        assert(dataReqPool[1].isApproved);
    }
    
    function testApproveTaskResult() public payable {
        require(msg.value >= 0.05 ether, "0.05 ETH needed");
        address playerAddr = 0xf5b475Fc347C4BA87E863f0b5269db08Af18f248;
        testInitiateTaskStatus(3);
        
        approveTaskResult(0);

        assert(playerPool[0].status == 4);
        assert(task.numPlayers == 0);
        assert(task.maxPlayers == 1);
        assert(balanceOf[playerAddr] == 500 ether);
    }
    
    function testRejectTaskResult() public  {
        testInitiateTaskStatus(3);
        
        rejectTaskResult(0);

        assert(playerPool[0].status == 99);
        assert(task.numPlayers == 0);
        assert(task.maxPlayers == 2);
    }
}
