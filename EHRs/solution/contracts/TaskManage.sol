/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./Owned.sol";
import "./SafeMath.sol";

contract TaskManage is Owned {
    using SafeMath for uint256;

    /* struct for a tool provider */
    struct Player {
        address addr;
        uint    dataInputId;
        uint    dataOutputId;
        int     result;
        uint8   status;
        uint    timeStamp;
    }

    struct Task {
        string  taskName;
        string  toolName;
        uint8   numPlayers;
        uint8   maxPlayers;
        uint    playerSalary;
        bool    isFinished;
        uint    timeStamp;
    }
    
    Task public task;
    Player[] public playerPool;
    bool   public isTaskInitialized;

    modifier validTask() {
        require(isTaskInitialized,
                "Task not initialized");
        _;
    }

    modifier validPlayer(uint playerId) {
        require(playerId < playerPool.length,
                "Invalid playerId");
        _;
    }

    function initiateTask (string toolName, 
                           uint8 maxPlayers,
                           uint playerSalary)
        public payable onlyOwner {
        require(!isTaskInitialized,
                "Task initialized");
        require(maxPlayers > 0,
                "Player must > 0");
        require(msg.value >= playerSalary.mul(maxPlayers),
                "Not enough ETH");
        task.taskName = "AI Computation";
        task.toolName = toolName;
        task.numPlayers = 0;
        task.maxPlayers = maxPlayers;
        task.playerSalary = playerSalary;
        task.isFinished = false;
        task.timeStamp = now;
        isTaskInitialized = true;
    }

    function submitPlayerRequest(uint dataInputId)
        public validTask returns (uint playerId) {
        require(msg.sender != owner,
                "Owner is player");
        require(task.numPlayers < task.maxPlayers,
                "Max players reached");
        playerPool.push(Player(msg.sender, dataInputId, 0, 0, 1, now));
        return playerPool.length - 1;
    }
    
    function approvePlayerRequest(uint playerId, uint dataOutputId)
        public onlyOwner validPlayer(playerId) validTask {
        require(task.numPlayers < task.maxPlayers,
                "Max players reached");
        playerPool[playerId].dataOutputId = dataOutputId;
        playerPool[playerId].status = 2;
        playerPool[playerId].timeStamp = now;
        task.numPlayers += 1;
    }

    function submitPlayerResult(uint playerId, int result) 
        public validPlayer(playerId) validTask {
        require(msg.sender == playerPool[playerId].addr,
                "Not origin player");
        require(playerPool[playerId].status == 2,
                "Invalid status");
        playerPool[playerId].result = result;
        playerPool[playerId].status = 3;
        playerPool[playerId].timeStamp = now;
    }

    function approvePlayerResult(uint playerId) 
        public onlyOwner validPlayer(playerId) validTask {
        require(playerPool[playerId].status == 3,
                "Invalid status");
        playerPool[playerId].status = 4;
        task.numPlayers -= 1;
        task.maxPlayers -= 1;
        playerPool[playerId].addr.transfer(task.playerSalary);
        if (0 == task.maxPlayers)
            task.isFinished = true;
    }

    function rejectPlayerResult(uint playerId) 
        public onlyOwner validPlayer(playerId) validTask {
        require(playerPool[playerId].status == 3,
                "Invalid status");
        playerPool[playerId].status = 99;
        playerPool[playerId].timeStamp = now;
        task.numPlayers -= 1;
    }

    function getPlayerCount()
        public view returns (uint count) {
        return playerPool.length;
    }
}