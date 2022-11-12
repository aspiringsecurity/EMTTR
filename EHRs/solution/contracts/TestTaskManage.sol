/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./TaskManage.sol";

contract TestTaskManage is TaskManage {
    address playerAddr = 0xf5b475Fc347C4BA87E863f0b5269db08Af18f248;

    function testInitiateTask() public payable {
        require(msg.value >= 0.1 ether, "0.1 ETH is required for testing");
        isTaskInitialized = false;
        task = Task({taskName: "", toolName: "",
                     numPlayers: 0, maxPlayers: 0, playerSalary: 0 ether,
                     isFinished: false, timeStamp: 0});
        
        initiateTask("ADMET", 2, 0.05 ether);
        
        assert(address(this).balance == 0.1 ether);
        assert(keccak256(bytes(task.taskName)) == keccak256("AI Computation"));
        assert(task.maxPlayers == 2);
        assert(!task.isFinished);
        assert(isTaskInitialized);
    }
    
    function testSubmitPlayerRequest() public {
        uint playerId;
        task = Task({taskName: "AI Computation", toolName: "ADMET",
                     numPlayers: 0, maxPlayers: 2, playerSalary: 0.05 ether,
                     isFinished: false, timeStamp: 1537142012});
        isTaskInitialized = true;
        owner = playerAddr;
        
        playerId = submitPlayerRequest(1);
        
        assert(playerId == playerPool.length - 1);
        assert(playerPool[playerId].dataInputId == 1);
        assert(playerPool[playerId].addr == msg.sender);
        assert(playerPool[playerId].status == 1);
    }
    
    function testApprovePlayerRequest() public {
        /* Initiate Task */
        task = Task({taskName: "AI Computation", toolName: "ADMET",
                     numPlayers: 0, maxPlayers: 2, playerSalary: 0.05 ether,
                     isFinished: false, timeStamp: 1537142012});
        isTaskInitialized = true;
        owner = msg.sender;
        /* Clean player pool */
        while (playerPool.length > 0)
        {
            delete playerPool[playerPool.length - 1];
            playerPool.length--;
        }
        /* Initiate player */
        playerPool.push(Player(playerAddr, 1, 0, 0, 1, now));
        
        approvePlayerRequest(0, 2);

        assert(playerPool[0].dataOutputId == 2);
        assert(playerPool[0].status == 2);
        assert(task.numPlayers == 1);
        
    }
    
    function testSubmitPlayerResult() public {
        /* Initiate Task */
        task = Task({taskName: "AI Computation", toolName: "ADMET",
                     numPlayers: 0, maxPlayers: 2, playerSalary: 0.05 ether,
                     isFinished: false, timeStamp: 1537142012});
        isTaskInitialized = true;
        owner = msg.sender;
        /* Clean player pool */
        while (playerPool.length > 0)
        {
            delete playerPool[playerPool.length - 1];
            playerPool.length--;
        }
        /* Initiate player */
        playerPool.push(Player(msg.sender, 1, 2, 0, 2, now));
        task.numPlayers = 1;
        
        submitPlayerResult(0, 0.5 ether);

        assert(playerPool[0].result == 0.5 ether);
        assert(playerPool[0].status == 3);
    }
    
    function testApprovePlayerResult() public payable {
        require(msg.value >= 0.05 ether, "0.05 ETH is required for testing");
        /* Initiate Task */
        task = Task({taskName: "AI Computation", toolName: "ADMET",
                     numPlayers: 0, maxPlayers: 2, playerSalary: 0.05 ether,
                     isFinished: false, timeStamp: 1537142012});
        isTaskInitialized = true;
        owner = msg.sender;
        /* Clean player pool */
        while (playerPool.length > 0)
        {
            delete playerPool[playerPool.length - 1];
            playerPool.length--;
        }
        /* Initiate player */
        playerPool.push(Player(playerAddr, 1, 2, 0.5 ether, 3, now));
        task.numPlayers = 1;
        
        approvePlayerResult(0);

        assert(playerPool[0].status == 4);
        assert(task.numPlayers == 0);
        assert(task.maxPlayers == 1);
    }
    
    function testRejectPlayerResult() public {
        /* Initiate Task */
        task = Task({taskName: "AI Computation", toolName: "ADMET",
                     numPlayers: 0, maxPlayers: 2, playerSalary: 0.05 ether,
                     isFinished: false, timeStamp: 1537142012});
        isTaskInitialized = true;
        owner = msg.sender;
        /* Clean player pool */
        while (playerPool.length > 0)
        {
            delete playerPool[playerPool.length - 1];
            playerPool.length--;
        }
        /* Initiate player */
        playerPool.push(Player(playerAddr, 1, 2, 0.5 ether, 3, now));
        task.numPlayers = 1;
        
        rejectPlayerResult(0);

        assert(playerPool[0].status == 99);
        assert(task.numPlayers == 0);
        assert(task.maxPlayers == 2);
    }

    function testGetPlayerCount() public {
        while (playerPool.length > 0)
        {
            delete playerPool[playerPool.length - 1];
            playerPool.length--;
        }

        testSubmitPlayerRequest();
        
        assert(getPlayerCount() == 1);
    }
}
