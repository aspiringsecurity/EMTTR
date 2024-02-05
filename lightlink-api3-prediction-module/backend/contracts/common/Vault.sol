//SPDX-License-Identifier:MIT

pragma solidity ^0.8.0;

import "../interfaces/IERC20.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

/// @notice Vault to store reserveFee of each MarketHandler when its concluded.
contract PM_Vault is Ownable {
    /// @notice The payment token address ie USDC
    IERC20 public immutable I_USDC_CONTRACT;

    /// @param _usdcAddress Address assigned to the immutable USDC
    constructor(address _usdcAddress) {
        I_USDC_CONTRACT = IERC20(_usdcAddress);
    }

    /// @notice Get the current USDC balance of the vault
    function currentBalance() external view returns (uint256) {
        return I_USDC_CONTRACT.balanceOf(address(this));
    }

    /// @notice Transfer all the USDC present in the contract to the owner
    function sendUSDC() external {
        uint256 balance = I_USDC_CONTRACT.balanceOf(address(this));
        I_USDC_CONTRACT.transfer(owner(), balance);
    }
}
