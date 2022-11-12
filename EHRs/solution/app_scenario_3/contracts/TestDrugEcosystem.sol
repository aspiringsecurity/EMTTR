/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
pragma solidity ^0.4.24;

import "./DrugEcosystem.sol";

contract TestDrugEcosystem is DrugEcosystem {
    function testCreateTask() public {
        uint taskId = createTask();
        
        assert(taskId == taskPool.length - 1);
        DrugTask newTask = DrugTask(taskPool[taskId]);
        assert(newTask.owner() == msg.sender);
    }
    
    function testGetTaskCount() public {
        while (taskPool.length > 0)
        {
            delete taskPool[taskPool.length - 1];
            taskPool.length--;
        }

        createTask();
        
        assert(getTaskCount() == 1);
    }
}
