// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import "wormhole-solidity-sdk/WormholeRelayerSDK.sol";

contract Spoke is TokenSender, TokenReceiver {
    uint256 constant GAS_LIMIT = 250_000;

    // This value is larger because a request must be sent back 
    uint256 constant GAS_LIMIT_FOR_WITHDRAWS_AND_BORROWS = 300_000; 

    enum Action {DEPOSIT, WITHDRAW, BORROW, REPAY}

    uint16 hubChain;
    address hubAddress;

    constructor(uint16 _hubChain, address _hubAddress, address _wormholeRelayer, address _tokenBridge, address _wormhole)
        TokenBase(_wormholeRelayer, _tokenBridge, _wormhole)
    {
        hubChain = _hubChain;
        hubAddress = _hubAddress;
    }

    function quoteDeposit() public view returns (uint256 cost) {
        uint256 deliveryCost;
        (deliveryCost,) = wormholeRelayer.quoteEVMDeliveryPrice(hubChain, 0, GAS_LIMIT);
        cost = deliveryCost + wormhole.messageFee();
    }

    function quoteBorrow(uint256 receiverValueForReturnDelivery) public view returns (uint256 cost) {
        (cost,) = wormholeRelayer.quoteEVMDeliveryPrice(hubChain, receiverValueForReturnDelivery, GAS_LIMIT_FOR_WITHDRAWS_AND_BORROWS);
    }

    function quoteRepay() public view returns (uint256 cost) {
        uint256 deliveryCost;
        (deliveryCost,) = wormholeRelayer.quoteEVMDeliveryPrice(hubChain, 0, GAS_LIMIT);
        cost = deliveryCost + wormhole.messageFee();
    }

    function quoteWithdraw(uint256 receiverValueForReturnDelivery) public view returns (uint256 cost) {
        (cost,) = wormholeRelayer.quoteEVMDeliveryPrice(hubChain, receiverValueForReturnDelivery, GAS_LIMIT_FOR_WITHDRAWS_AND_BORROWS);
    }

    function deposit(address tokenAddress, uint256 amount) public payable {
        IERC20(tokenAddress).transferFrom(msg.sender, address(this), amount);
        sendTokenWithPayloadToEvm(hubChain, hubAddress, abi.encode(Action.DEPOSIT, msg.sender), 0, GAS_LIMIT, tokenAddress, amount);
    }

    function withdraw(address tokenAddress, uint256 amount, uint256 receiverValueForReturnDelivery) public payable {
        wormholeRelayer.sendPayloadToEvm{value: msg.value}(hubChain, hubAddress, abi.encode(Action.WITHDRAW, msg.sender, tokenAddress, amount), receiverValueForReturnDelivery, GAS_LIMIT_FOR_WITHDRAWS_AND_BORROWS);
    }

    function borrow(address tokenAddress, uint256 amount, uint256 receiverValueForReturnDelivery) public payable {
        wormholeRelayer.sendPayloadToEvm{value: msg.value}(hubChain, hubAddress, abi.encode(Action.BORROW, msg.sender, tokenAddress, amount), receiverValueForReturnDelivery, GAS_LIMIT_FOR_WITHDRAWS_AND_BORROWS);
    }

    function repay(address tokenAddress, uint256 amount) public payable {
        IERC20(tokenAddress).transferFrom(msg.sender, address(this), amount);
        sendTokenWithPayloadToEvm(hubChain, hubAddress, abi.encode(Action.REPAY, msg.sender), 0, GAS_LIMIT, tokenAddress, amount);
   }

    function receivePayloadAndTokens(
        bytes memory payload,
        TokenReceived[] memory receivedTokens,
        bytes32 sourceAddress, 
        uint16 sourceChain,
        bytes32 deliveryHash
    ) internal override onlyWormholeRelayer isRegisteredSender(sourceChain, sourceAddress) replayProtect(deliveryHash) {
        require(receivedTokens.length == 1, "Expecting one transfer");
        TokenReceived memory receivedToken = receivedTokens[0];
            
        (address user) = abi.decode(payload, (address));
        IERC20(receivedToken.tokenAddress).transfer(user, receivedToken.amount);

        // send any refund back to the user
        user.call{value: msg.value}("");
    }
}
