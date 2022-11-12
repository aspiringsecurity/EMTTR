/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./Legitimation.sol";

contract TestLegitimation is Legitimation {
    function testSignAgreement() public {
        string memory fileURL = "https://drive.google.com/open?id=12345_abcdefg-HIJK";
        string memory hashCode = "c8e1f08945eb55208947cb1c76d911ed8d5d44b74b2d02f4e736ef4aef7ab7c8";
        string memory signature = "0xd42dd19960f70fecdb50977568146aa46e33c9c42a6205d08fd0356b4c0f7f10592ccc0f291d8406fb5d7609fbd2d4bcc72ba28f7c84b40e5ca3d81d5442e1a91b";
        if (LegiFiles[msg.sender].isSigned)
            delete LegiFiles[msg.sender];
        
        signAgreement(fileURL, hashCode, signature);
        
        assert(keccak256(bytes(LegiFiles[msg.sender].fileURL)) == keccak256(bytes(fileURL)));
        assert(LegiFiles[msg.sender].isSigned);
    }
}
