// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";

contract MockUSDC is ERC20 {
    function decimals() public pure override returns (uint8) {
        return 6;
    }

    constructor() ERC20("USDC", "US Dollar Coin") {}

    function mint(address to, uint256 amount) public {
        _mint(to, amount);
    }
}
