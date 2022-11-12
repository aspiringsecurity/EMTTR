pragma solidity ^0.4.24;

import "./Token.sol";

contract Ecosystem is Token {
    uint256 private taskCount = 0;
    uint256 private resultCount = 0;

    struct Task {
        address owner;
        string title;
        uint256 deposit;
        string dataURL;
        string dataHash;
        uint timeStamp;
        int maxResult;
    }

    struct Result {
        address owner;
        uint256 task_id;
        int result;
        string dataURL;
        string dataHash;
        uint timeStamp;
    }

    mapping(uint256 => Task) tasks;
    mapping(uint256 => Result) results;

    constructor(string name, string symbol, uint8 decimals,uint256 amount) public
    {
        super.deployToken(name,symbol,decimals,amount);
    }

    function initToken(string name, string symbol, uint8 decimals,uint256 amount) public {
        super.deployToken(name,symbol,decimals,amount);
    }
    

    function numTask() public view returns (uint256) {
        return taskCount;
    }

    function numResult() public view returns (uint256) {
        return resultCount;
    }

    function createTask(address owner,string title, uint256 deposit, string dataURL, string dataHash, int maxResult) public {
        require(deposit > 0, "deposit has to be greater than 0");
        require(balances[msg.sender] > 0, "insuffcient balance");
        //tasks[taskCount].owner = msg.sender;
        taskCount = taskCount+1;
        tasks[taskCount].owner = owner;
        tasks[taskCount].title = title;
        _burn(owner,deposit);
        tasks[taskCount].deposit = deposit;
        tasks[taskCount].dataURL = dataURL;
        tasks[taskCount].dataHash = dataHash;
        tasks[taskCount].maxResult = maxResult;
        tasks[taskCount].timeStamp = now;
    }

    function submitResult(address owner,uint256 task_id, int result, string dataURL, string dataHash) public {
        resultCount = resultCount+1;
        results[resultCount].owner = owner;
        results[resultCount].task_id = task_id;
        results[resultCount].result = result;
        results[resultCount].dataURL = dataURL;
        results[resultCount].dataHash = dataHash;
        results[resultCount].timeStamp = now;
    }

    function getTask(uint256 _id) public view returns (address, string, uint256, string, string, uint) {
        return (tasks[_id].owner, tasks[_id].title, tasks[_id].deposit,  tasks[_id].dataURL,  tasks[_id].dataHash, tasks[_id].timeStamp);
    }

    function getResult(uint256 _id) public view returns (address, uint256, int, string, string) {
        return (results[_id].owner, results[_id].task_id, results[_id].result, results[_id].dataURL, results[_id].dataHash);
    }


    function getTaskResult(uint256 _id) public view returns (uint256[]) {
        uint256[] memory outArray_ = new uint256[](8);
        uint count = 0;
        uint max = resultCount+1;
        for (uint i=1; i<max;i++) {
            if (results[i].task_id == _id) {
                outArray_[count] = i;
                count = count+1;
            }
        }   
        return (outArray_);
    }

    function awardFirstPlayer(uint256 _id) public {
        uint max = resultCount+1;
        for (uint i=1; i<max;i++) {
            if (results[i].task_id == _id) {
                // presume approve
                _mint(results[i].owner,tasks[_id].deposit);
                delete tasks[_id];
                break;
            }
        } 
    }  

    function cancelTask(uint256 _id) public {
        _mint(tasks[_id].owner,tasks[_id].deposit);
        delete tasks[_id];
    }




}