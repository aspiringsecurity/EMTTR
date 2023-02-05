//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.0;

import "@matterlabs/zksync-contracts/l2/system-contracts/TransactionHelper.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";

import "./interfaces/IPlugin.sol";

contract Plugin is IPlugin {
    using TransactionHelper for Transaction;

    struct Limit {
        address spender;
        uint256 amount;
    }

    mapping(address => mapping (address => uint )) public activeLimits;

    function getActiveLimitsLength (address aaAddr) public view returns (uint) {
        return 0;
    }

    function isValid(Transaction calldata _transaction)
        public
        view
        override
        returns (bool)
    {
        bytes32 txHash = _transaction.encodeHash();

        address accountAddr = address(uint160(_transaction.from));

        require(_transaction.signature.length == 65, "Signature length is incorrect");

        address signerAddr = ECDSA.recover(txHash, _transaction.signature[0:65]);

        bool _valid = true;

        _valid = activeLimits[accountAddr][signerAddr] > _transaction.reserved[1]; // compare with message value

        return _valid;
    }

    function addWallets(Limit[] calldata _newLimits) external {
        for(uint i = 0; i < _newLimits.length; ++i){
            activeLimits[msg.sender][_newLimits[i].spender] = _newLimits[i].amount;
        }
    }

    function removeWallets(address[] calldata _wallets) external {
        for(uint i = 0; i < _wallets.length; ++i){
            activeLimits[msg.sender][_wallets[i]] = 0;
        }
    }
}
