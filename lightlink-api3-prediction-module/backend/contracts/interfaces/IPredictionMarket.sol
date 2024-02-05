// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts (last updated v4.6.0) (token/ERC20/IERC20.sol)

pragma solidity ^0.8.0;

interface IPredictionMarket {
    function trackProgress(
        uint256 _id,
        address _caller,
        int256 _amountYes,
        int256 _amountNo
    ) external;
}
