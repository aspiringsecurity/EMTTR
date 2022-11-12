/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./DrugTask.sol";

contract DrugEcosystem {
    DrugTask[] public taskPool;

    function createTask() public returns (uint index) {
        DrugTask newTask = new DrugTask();
        newTask.setOwner(msg.sender);
        taskPool.push(newTask);
        return taskPool.length - 1;
    }

    function getTaskCount() public view returns (uint count) {
        return taskPool.length;
    }
}
