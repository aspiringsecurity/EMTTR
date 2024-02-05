// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

interface IMarketHandler {
    function swapTokenNoWithYes(uint256) external;

    function swapTokenYesWithNo(uint256) external;

    function buyNoToken(uint256) external;

    function buyYesToken(uint256) external;

    function sellNoToken(uint256) external;

    function sellYesToken(uint256) external;

    function concludePrediction_3(bool winner) external;
}
