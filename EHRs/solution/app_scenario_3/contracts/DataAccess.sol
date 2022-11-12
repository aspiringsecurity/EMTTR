/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./Owned.sol";

contract DataAccess {
    /* struct for a new request */
    struct Request {
        address addr;
        string  encryptCode;
        string  dataURL;
        string  dataHash;
        bool    isApproved;
        uint    timeStamp;
        address dataOwner;
    }

    /* store all data requests */
    Request[] public dataReqPool;

    function sendDataRequest(address dataOwner, string encryptCode)
        public returns (uint index) {
        require(bytes(encryptCode).length > 0);
        dataReqPool.push(Request(msg.sender, encryptCode, "", "", false, now, dataOwner));
        return dataReqPool.length - 1;
    }
    
    function approveDataRequest(uint index, string dataURL, string dataHash)
        public {
        require(index < dataReqPool.length,
                "Invalid index");
        require(!dataReqPool[index].isApproved,
                "Request approved");
        require(msg.sender == dataReqPool[index].dataOwner,
                "Not data owner");
        require(bytes(dataURL).length > 0);
        require(bytes(dataHash).length > 0);
        
        dataReqPool[index].dataURL = dataURL;
        dataReqPool[index].dataHash = dataHash;
        dataReqPool[index].isApproved = true;
        dataReqPool[index].timeStamp = now;
    }

    function getRequestCount()
        public view returns (uint count) {
        return dataReqPool.length;
    }
}