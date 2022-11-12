/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./DataAccess.sol";

contract TestDataAccess is DataAccess {
    function testSendDataRequest() public {
        uint dataId;
        address dataOwner = 0xf5b475Fc347C4BA87E863f0b5269db08Af18f248;
        string memory encryptCode = "03c57256ec2faf84828287710dfea24e5eeba1c36cb42c92d530aa460b7af1e462";

        dataId = sendDataRequest(dataOwner, encryptCode);
        
        assert(dataId == dataReqPool.length - 1);
        assert(dataReqPool[dataId].addr == msg.sender);
        assert(dataReqPool[dataId].dataOwner == dataOwner);
        assert(keccak256(bytes(dataReqPool[dataId].encryptCode)) == keccak256(bytes(encryptCode)));
        assert(!dataReqPool[dataId].isApproved);
    }
    
    function testApproveDataRequest() public {
        string memory dataURL = "https://drive.google.com/open?id=12345_abcdefg-HIJK";
        string memory dataHash = "c8e1f08945eb55208947cb1c76d911ed8d5d44b74b2d02f4e736ef4aef7ab7c8";
        testGetRequestCount();
        
        approveDataRequest(0, dataURL, dataHash);

        assert(keccak256(bytes(dataReqPool[0].dataURL)) == keccak256(bytes(dataURL)));
        assert(dataReqPool[0].isApproved);
    }
    
    function testGetRequestCount() public {
        while (dataReqPool.length > 0)
        {
            delete dataReqPool[dataReqPool.length - 1];
            dataReqPool.length--;
        }

        testSendDataRequest();
        
        assert(getRequestCount() == 1);
    }
}

