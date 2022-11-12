pragma solidity ^0.4.24;

import "./Token.sol";

contract Ecosystem is Token {

    struct Task {
        address owner;
        string title;
        // status
        //  0 : pending (efficient drug delivery)
        //  1 : approved (efficient drug delivery)
        //  2 : completed (efficient drug delivery)
        //  3 : pending for clinical trials - not available
        //  4 : approved for clinical trials - not available
        //  5 : recruiting - not available
        //  6 : completed  - not available
        //  7 : terminated - not available
        int status;
        uint256 deposit;
        // see docs
        string metadataURL;
        string metadataHash;
        uint timeStamp;
        uint256 lat;
        uint256 long;
        bool onsite;
    }

    struct Result {
        address owner;
        // status
        // 0 : pending
        // 1 : approved
        int status;
        uint256 task_id;
        // assume 1-100
        int result;
        // see docs
        string metadataURL;
        string metadataHash;
        uint timeStamp;
    }

    address contractOwner = 0xCA35b7d915458EF540aDe6068dFe2F44E8fa733c;

    mapping(uint256 => Task) tasks;
    mapping(uint256 => Result) results;

    uint256 private taskCount = 0;
    uint256 private resultCount = 0;

    function numTask() public view returns (uint256) {
        return taskCount;
    }

    function getSender() public view returns (address) {
        return msg.sender;
    }

    function numResult() public view returns (uint256) {
        return resultCount;
    }

    function createTask(address owner,string title, uint256 deposit, string metadataURL, string metadataHash) public {
        require(deposit > 0, "deposit has to be greater than 0");
        require(balances[msg.sender] > 0, "insuffcient balance");
        //if (msg.sender != contractOwner) {
            //require(msg.sender == owner, "invoke by unautorized user");
        //}
        taskCount = taskCount+1;
        tasks[taskCount].owner = owner;
        tasks[taskCount].title = title;
        _burn(owner,deposit);
        tasks[taskCount].status = 0;
        tasks[taskCount].deposit = deposit;
        tasks[taskCount].metadataURL = metadataURL;
        tasks[taskCount].metadataHash = metadataHash;
        tasks[taskCount].timeStamp = now;
    }

    function getTask(uint256 _id) public view returns (address, string, uint256, string, uint, int) {
        return (tasks[_id].owner, tasks[_id].title, tasks[_id].deposit,  tasks[_id].metadataURL, tasks[_id].timeStamp, tasks[_id].status);
    }

    function getResult(uint256 _id) public view returns (address, uint256, int, int, string) {
        return (results[_id].owner, results[_id].task_id, results[_id].status, results[_id].result, results[_id].metadataURL);
    } 

    function approveTask(uint256 _id) public {
        //if (msg.sender != contractOwner) {
            //require(msg.sender == tasks[_id].owner, "invoke by unautorized user");
        //}
        setTaskStatus(_id,1);
    }

    function setTaskStatus(uint256 _id, int _status) public {
        //if (msg.sender != contractOwner) {
            //require(msg.sender == tasks[_id].owner, "invoke by unautorized user");
        //}
        tasks[_id].status = _status;
    }

    function cancelTask(uint256 _id) public {
        require(tasks[_id].status != 0, "cannot cancel ongoing task");
        _mint(tasks[_id].owner,tasks[_id].deposit);
        delete tasks[_id];
    }


    function submitResult(address owner, uint256 task_id, int result, string metadataURL, string metadataHash) public {
        require(tasks[task_id].status == 1, "task is not ready to submit");
        require(result > 0, "result has to be greater than 0");
        //if (msg.sender != contractOwner) {
            //require(msg.sender == owner, "invoke by unautorized user");
        //}
        resultCount = resultCount+1;
        results[resultCount].owner = owner;
        results[resultCount].status = 0;
        results[resultCount].task_id = task_id;
        results[resultCount].result = result;
        results[resultCount].metadataURL = metadataURL;
        results[resultCount].metadataHash = metadataHash;
        results[resultCount].timeStamp = now;
    }


    function setResultStatus(uint256 _id, int _status) public {
        // only task owner can change its status
        //if (msg.sender != contractOwner) {
            //require(msg.sender == tasks[_id].owner, "invoke by unautorized user");
        //}
        results[_id].status = _status;
    }

    function approveResult(uint256 _id) public {
        // only task owner can change its status
        //if (msg.sender != contractOwner) {
            //require(msg.sender == tasks[_id].owner, "invoke by unautorized user");
        //}
        setResultStatus(_id, 1);
    }

    function findResult(uint256 _task_id) public view returns (uint256[]) {
        uint256[] memory outArray_ = new uint256[](8);
        uint count = 0;
        uint max = resultCount+1;
        for (uint i=1; i<max;i++) {
            if (results[i].task_id == _task_id) {
                outArray_[count] = i;
                count = count+1;
            }
        }   
        return (outArray_);
    }

    
    function award(uint256 _task_id) public {
        //if (msg.sender != contractOwner) {
            //require(msg.sender == tasks[_task_id].owner, "invoke by unautorized user");
        //}
        require(tasks[_task_id].status == 1, "invalid status");
        uint max = resultCount+1;
        int highestResult = 0;
        address playerId = 0x0;
        for (uint i=1; i<max;i++) {
            if (results[i].status == 1)  {
               if (results[i].task_id == _task_id) {
                   if (results[i].result > highestResult) {
                       highestResult = results[i].result;
                       playerId =  results[i].owner;
                   }
               } 
            }
        }
        require(playerId != 0x0, "not found player");
        require(highestResult > 0, "result has to be greater then 0 ");
        _mint(playerId,tasks[_task_id].deposit);
        tasks[_task_id].deposit = 0;
        tasks[_task_id].status = 2;
        
    }







}