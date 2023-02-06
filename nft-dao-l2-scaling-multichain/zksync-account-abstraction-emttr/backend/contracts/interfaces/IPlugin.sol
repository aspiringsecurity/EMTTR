//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@matterlabs/zksync-contracts/l2/system-contracts/TransactionHelper.sol";

interface IPlugin {
    function isValid(Transaction calldata _tx) external view returns (bool);
}
