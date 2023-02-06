// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@matterlabs/zksync-contracts/l2/system-contracts/Constants.sol";
import "@matterlabs/zksync-contracts/l2/system-contracts/SystemContractsCaller.sol";

contract AAFactory {
    bytes32 public aaBytecodeHash;

    event CreatedNewAA (address newAA);

    constructor(bytes32 _aaBytecodeHash) {
        aaBytecodeHash = _aaBytecodeHash;
    }

    function deployAccount(bytes32 salt, address owner)
        external
        returns (address accountAddress)
    {
        bytes memory returnData = SystemContractsCaller.systemCall(
            uint32(gasleft()),
            address(DEPLOYER_SYSTEM_CONTRACT),
            0,
            abi.encodeCall(
                DEPLOYER_SYSTEM_CONTRACT.create2Account,
                (salt, aaBytecodeHash, abi.encode(owner))
            )
        );

        (accountAddress, ) = abi.decode(returnData, (address, bytes));

        emit CreatedNewAA(accountAddress);
    }
}
