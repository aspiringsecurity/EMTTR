// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@matterlabs/zksync-contracts/l2/system-contracts/interfaces/IAccount.sol";
import "@matterlabs/zksync-contracts/l2/system-contracts/TransactionHelper.sol";

import "@openzeppelin/contracts/interfaces/IERC1271.sol";

// Used for signature validation
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

// Access zkSync system contracts, in this case for nonce validation vs NONCE_HOLDER_SYSTEM_CONTRACT
import "@matterlabs/zksync-contracts/l2/system-contracts/Constants.sol";
// to call non-view method of system contracts
import "@matterlabs/zksync-contracts/l2/system-contracts/SystemContractsCaller.sol";

import "./interfaces/IPlugin.sol";

contract PluginableAccount is IAccount, IERC1271 {
    // to get transaction hash
    using TransactionHelper for Transaction;

    event NewPluginActivated (address plugin);

    // state variables for account owners
    address public owner;

    bytes4 constant EIP1271_SUCCESS_RETURN_VALUE = 0x1626ba7e;

    address[] public activePlugins;

    modifier onlyBootloader() {
        require(
            msg.sender == BOOTLOADER_FORMAL_ADDRESS,
            "Only bootloader can call this method"
        );
        // Continure execution if called from the bootloader.
        _;
    }

    constructor(address _owner) {
        owner = _owner;
    }

    function validateTransaction(
        bytes32,
        bytes32 _suggestedSignedHash,
        Transaction calldata _transaction
    ) external payable override onlyBootloader {
        _validateTransaction(_suggestedSignedHash, _transaction);
    }

    function _validateTransaction(
        bytes32 _suggestedSignedHash,
        Transaction calldata _transaction
    ) internal {
        // Incrementing the nonce of the account.
        // Note, that reserved[0] by convention is currently equal to the nonce passed in the transaction
        SystemContractsCaller.systemCall(
            uint32(gasleft()),
            address(NONCE_HOLDER_SYSTEM_CONTRACT),
            0,
            abi.encodeCall(
                INonceHolder.incrementMinNonceIfEquals,
                (_transaction.reserved[0])
            )
        );

        bytes32 txHash;
        // While the suggested signed hash is usually provided, it is generally
        // not recommended to rely on it to be present, since in the future
        // there may be tx types with no suggested signed hash.
        if (_suggestedSignedHash == bytes32(0)) {
            txHash = _transaction.encodeHash();
        } else {
            txHash = _suggestedSignedHash;
        }

        bool isOwner = _isSignedByOwner(txHash, _transaction.signature);

        if (isOwner) {

        } else {
            if (
                address(uint160(_transaction.to)) == address(this) ||
                activePlugins.length == 0
            ) {
                require(false, "Not signed by owner - plugin length is zero");
            }

            // Iterate over all plugins
            for (uint256 i = 0; i < activePlugins.length; i += 1) {
                if (address(uint160(_transaction.to)) == activePlugins[i]) {
                    revert("Only owner can interact with Plugin management");
                }

                if (!IPlugin(activePlugins[i]).isValid(_transaction)) {
                    revert("Plugin validation failed");
                }
            }
        }

        
    }

    function activatePlugin(address _newPlugin) public {
        require(msg.sender == address(this) || msg.sender == owner, "Only AA or Owner can call this method");

        activePlugins.push(_newPlugin);

        emit NewPluginActivated(_newPlugin);
    }

    function removePlugin(address _plugin) public {
        require(msg.sender == address(this) || msg.sender == owner, "Only AA or Owner can call this method");

        for (uint256 i = 0; i < activePlugins.length; i += 1) {
            if (activePlugins[i] == _plugin) {
                activePlugins[i] = activePlugins[activePlugins.length - 1];
                activePlugins.pop();
            }
        }
    }

    function _isSignedByOwner(bytes32 _txHash, bytes calldata _signature)
        internal
        view
        returns (bool)
    {
        // The signature is the concatenation of the ECDSA signatures of the owners
        // Each ECDSA signature is 65 bytes long. That means that the combined signature is 130 bytes long.
        require(_signature.length == 65, "Signature length is incorrect");

        address recoveredAddr = ECDSA.recover(_txHash, _signature[0:65]);

        return recoveredAddr == owner;
    }

    function executeTransaction(
        bytes32,
        bytes32,
        Transaction calldata _transaction
    ) external payable override onlyBootloader {
        _executeTransaction(_transaction);
    }

    function _executeTransaction(Transaction calldata _transaction) internal {
        address to = address(uint160(_transaction.to));
        uint256 value = _transaction.reserved[1];
        bytes memory data = _transaction.data;

        if (to == address(DEPLOYER_SYSTEM_CONTRACT)) {
            // We allow calling ContractDeployer with any calldata
            SystemContractsCaller.systemCall(
                uint32(gasleft()),
                to,
                uint128(_transaction.reserved[1]), // By convention, reserved[1] is `value`
                _transaction.data
            );
        } else {
            bool success;
            assembly {
                success := call(
                    gas(),
                    to,
                    value,
                    add(data, 0x20),
                    mload(data),
                    0,
                    0
                )
            }
            require(success);
        }
    }

    function executeTransactionFromOutside(Transaction calldata _transaction)
        external
        payable
    {
        _validateTransaction(bytes32(0), _transaction);

        _executeTransaction(_transaction);
    }

    function isValidSignature(bytes32 _hash, bytes calldata _signature)
        public
        view
        override
        returns (bytes4)
    {
        if (_isSignedByOwner(_hash, _signature))
            return EIP1271_SUCCESS_RETURN_VALUE;
        else return 0xffffffff;
    }

    function payForTransaction(
        bytes32,
        bytes32,
        Transaction calldata _transaction
    ) external payable override onlyBootloader {
        bool success = _transaction.payToTheBootloader();
        require(success, "Failed to pay the fee to the operator");
    }

    function prePaymaster(
        bytes32,
        bytes32,
        Transaction calldata _transaction
    ) external payable override onlyBootloader {
        _transaction.processPaymasterInput();
    }

    receive() external payable {
        // If the bootloader called the `receive` function, it likely means
        // that something went wrong and the transaction should be aborted. The bootloader should
        // only interact through the `validateTransaction`/`executeTransaction` methods.
        assert(msg.sender != BOOTLOADER_FORMAL_ADDRESS);
    }
}
