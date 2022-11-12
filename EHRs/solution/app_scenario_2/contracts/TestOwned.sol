/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./Owned.sol";

contract TestOwned is Owned {
    function testSetOwner() public {
        owner = msg.sender;
        address newOwner = 0xf5b475Fc347C4BA87E863f0b5269db08Af18f248;
        
        setOwner(newOwner);
        
        assert(owner == 0xf5b475Fc347C4BA87E863f0b5269db08Af18f248);
    }
}
