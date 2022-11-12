/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

contract Legitimation {
    struct LegiFile {
        string fileURL;
        string hashCode;
        string signature;
        uint   timeStamp;
        bool   isSigned;
    }

    /* This creates an array with all legal info */
    mapping (address => LegiFile) public LegiFiles;

    modifier notSigned() {
        require(!LegiFiles[msg.sender].isSigned,
                "File signed");
        _;
    }

    /* This generates a public event on the blockchain that will notify clients */
    event SignLawAgreement(address indexed signee, string fileName, string hashCode, string signature, uint256 timeStamp);
    
    function signAgreement(string fileURL, string hashCode, string signature)
        public notSigned {
        require(bytes(fileURL).length > 0);
        require(bytes(hashCode).length > 0);
        require(bytes(signature).length > 0);

        LegiFiles[msg.sender] = LegiFile(fileURL, hashCode, signature, now, true);
        emit SignLawAgreement(msg.sender, fileURL, hashCode, signature, now);
    }
}
