// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "../interfaces/IMarketHandler.sol";
import "../interfaces/IPredictionMarket.sol";
import "../interfaces/IERC20.sol";

import "@openzeppelin/contracts/utils/Context.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/utils/Counters.sol";

error PM_IsClosedForTrading();
error PM_IsOpenForTrading();
error PM_InsufficientApprovedAmount();
error PM_TokenTransferFailed();
error PM_InsufficienTradeTokens();
error PM_InvalidAmountSet();
error PM_RewardsNotAvailable();
error PM_RewardAlreadyCollected();
error PM_UserDidNotWin();

/// @notice Responsible for all the trading activity for a prediction created.
contract PM_MarketHandler is Context, Ownable, IMarketHandler {
    using Counters for Counters.Counter;

    // reserveUSDC - reserveFEE = reserveYes + reserveNo
    /// @notice Total USDC collected.
    uint256 public reserveUSDC;
    /// @notice Total platform fee collected so far.
    uint256 public reserveFEE;
    /// @notice Total USDC collected against Yes tokens.
    uint256 public reserveYes;
    /// @notice Total USDC collected against No tokens.
    uint256 public reserveNo;

    /// @notice If rewards are ready to be claimed.
    bool public RewardsClaimable;

    /// @notice The context of the side that won.
    bool public winner;

    /// @notice The id alloted in the Trading contract for each prediction as predictionId.
    uint256 public immutable I_SELF_ID;
    /// @notice Price of 1 token of either side. Is a multiple of 10**4.
    uint256 public immutable I_BASE_PRICE;
    /// @notice When the market will close down.
    uint256 public immutable I_DEADLINE;
    /// @notice The decimal value set in the payment token. Is required to calculate the fee during any trading activity.
    uint256 public immutable I_DECIMALS;

    /// 100% = 10**decimals(), 0.1% = 10**decimals()/1000.
    /// This is the total fee and this further divided between the creator and the platform.
    uint256 public immutable I_FEE;

    /// @notice The interface for the payment token.
    IERC20 public immutable I_USDC_CONTRACT;
    /// @notice The vault address.
    address private I_VAULT_ADDRESS;

    IPredictionMarket private I_PREDICTION_MARKET_CONTRACT;

    /// @notice Variables to track the 'Yes' token and its holders
    /// @notice The current valid index where new address is to be pushed in the yesHolders array.
    Counters.Counter private yesIndex;
    ///@notice The array that holds all the address in possession of 'Yes' token.
    address[] private yesHolders;
    /// @notice Get the index of an address in the yesHolders array
    mapping(address => uint256) private yesTokenAddressToIndex;
    /// @notice Track the amount of 'Yes' tokens held by an address
    mapping(address => uint256) private YesBalances;

    /// @notice SAME AS ABOVE BUT FOR 'NO' TOKENS.
    Counters.Counter private noIndex;
    address[] private noHolders;
    mapping(address => uint256) private noTokenAddressToIndex;
    mapping(address => uint256) private NoBalances;

    /// @notice Check if a user collected their rewards to disable multiple withdrawls
    mapping(address => bool) public rewardCollected;

    // Events
    /// @notice When a trader swaps their 'Yes' for 'No' or vica versa.
    event SwapOrder(address indexed trader, int256 amountYes, int256 amountNo);
    /// @notice When a trader buys a token either 'Yes' or 'No'.
    event BuyOrder(address indexed trader, uint256 amountYes, uint256 amountNo);
    /// @notice When a trader is looking to withdraw from the prediction and collected their invested amount.
    event SellOrder(
        address indexed trader,
        uint256 amountYes,
        uint256 amountNo
    );
    /// @notice The bool value the tells the nature of the prediction result.
    event WinnerDeclared(bool winner);
    /// @notice When a trader successfully collect their rewards.
    event RewardCollected(address indexed user, uint256 amountWon);

    /// @notice Check if the collectRewards is open to be called by the winners.
    modifier isClaimable() {
        if (!RewardsClaimable) revert PM_RewardsNotAvailable();
        _;
    }

    /// @notice Check if the market is Open for trades or not.
    modifier isOpen() {
        if (block.timestamp > I_DEADLINE) revert PM_IsClosedForTrading();
        _;
    }

    /// @notice Check if the market is Closed for trades.
    modifier isClosed() {
        if (block.timestamp <= I_DEADLINE) revert PM_IsOpenForTrading();
        _;
    }

    /// @param _id The unique predictionId set in the parent Trading contract.
    // _fee * 0.01% of the tokens regardless of the decimals value. Should be a natural number N.
    /// @param _fee The multiple of 0.01% to declare the final trading fee the platform collects.
    /// @param _deadline The timestamp upto which the market is open for trades.
    // 10**6 = 1 USDC, 10**4 = 0.01 USDC or 1 Cent. Therefore base price = A x 1 cent.
    /// @param _basePrice Multiple of 1 cent. Price of either of the token.
    /// @param _usdcTokenAddress The payment token address.
    /// @param _vaultAddress The vault address that will collect the reserveFEE on conclude.
    constructor(
        uint256 _id,
        uint256 _fee,
        uint256 _deadline,
        uint256 _basePrice,
        address _usdcTokenAddress,
        address _vaultAddress
    ) {
        I_PREDICTION_MARKET_CONTRACT = IPredictionMarket(_msgSender());

        I_SELF_ID = _id;
        I_BASE_PRICE = _basePrice * 10 ** 4;
        I_DEADLINE = _deadline;
        I_VAULT_ADDRESS = _vaultAddress;
        IERC20 usdcContract = IERC20(_usdcTokenAddress);
        I_USDC_CONTRACT = usdcContract;
        I_DECIMALS = 10 ** usdcContract.decimals();
        I_FEE = (_fee * 10 ** usdcContract.decimals()) / 10 ** 4;

        yesIndex.increment();
        noHolders.push(address(0));
        noIndex.increment();
        yesHolders.push(address(0));
    }

    /// @notice No => Yes
    /// @notice Function To Swap 'No' tokens for 'Yes' tokens.
    /// @param _amount The amount of tokens to swap. Is a natural number N.
    function swapTokenNoWithYes(uint256 _amount) external override isOpen {
        uint256 equivalentUSDC = (_amount * I_BASE_PRICE) / I_DECIMALS;
        uint256 swapFee = getFee(equivalentUSDC);

        uint256 swapTokenDeduction = getFee(_amount);

        if (NoBalances[_msgSender()] < _amount)
            revert PM_InsufficienTradeTokens();

        NoBalances[_msgSender()] -= _amount;
        reserveNo -= equivalentUSDC;
        YesBalances[_msgSender()] += _amount - swapTokenDeduction;
        reserveYes += equivalentUSDC - swapFee;

        int256 amountYes = int256(_amount - swapTokenDeduction);
        int256 amountNo = int256(_amount);

        I_USDC_CONTRACT.transfer(I_VAULT_ADDRESS, swapFee);
        reserveFEE += swapFee;

        I_PREDICTION_MARKET_CONTRACT.trackProgress(
            I_SELF_ID,
            _msgSender(),
            amountYes,
            -1 * amountNo
        );
        emit SwapOrder(_msgSender(), amountYes, -1 * amountNo);
    }

    /// @notice Yes => No
    /// @notice SAME AS ABOVE BUT TO SWAP 'Yes' for 'No' tokens.
    function swapTokenYesWithNo(uint256 _amount) external override isOpen {
        uint256 equivalentUSDC = (_amount * I_BASE_PRICE) / I_DECIMALS;
        uint256 swapFee = getFee(equivalentUSDC);

        uint256 swapTokenDeduction = getFee(_amount);

        if (YesBalances[_msgSender()] < _amount)
            revert PM_InsufficienTradeTokens();

        NoBalances[_msgSender()] += _amount - swapTokenDeduction;
        reserveNo += equivalentUSDC - swapFee;
        YesBalances[_msgSender()] -= _amount;
        reserveYes -= equivalentUSDC;

        int256 amountYes = int256(_amount);
        int256 amountNo = int256(_amount - swapTokenDeduction);

        I_USDC_CONTRACT.transfer(I_VAULT_ADDRESS, swapFee);
        reserveFEE += swapFee;

        I_PREDICTION_MARKET_CONTRACT.trackProgress(
            I_SELF_ID,
            _msgSender(),
            -1 * amountYes,
            amountNo
        );
        emit SwapOrder(_msgSender(), -1 * amountYes, amountNo);
    }

    /// @notice To enable the purchase of _amount of 'No' tokens. The total fee is based on _amount * I_BASE_PRICE.
    /// @param _amount Total tokens the trader is looking to buy. Is a multiple of 10**decimals()
    function buyNoToken(uint256 _amount) external override isOpen {
        if (_amount < 1) revert();

        uint256 owedAmount = (_amount * I_BASE_PRICE) / I_DECIMALS;

        if (I_USDC_CONTRACT.allowance(_msgSender(), address(this)) < owedAmount)
            revert PM_InsufficientApprovedAmount();
        bool success = I_USDC_CONTRACT.transferFrom(
            _msgSender(),
            address(this),
            owedAmount
        );
        if (!success) revert PM_TokenTransferFailed();

        uint256 fee = getFee(owedAmount);
        I_USDC_CONTRACT.transfer(I_VAULT_ADDRESS, fee);

        reserveFEE += fee;
        reserveUSDC += owedAmount - fee;
        reserveNo += owedAmount - fee;

        uint256 finalAmount = _amount - getFee(_amount);
        NoBalances[_msgSender()] += finalAmount;

        if (noTokenAddressToIndex[_msgSender()] == 0) {
            uint256 index = noIndex.current();

            noTokenAddressToIndex[_msgSender()] = index;
            noHolders.push(_msgSender());

            noIndex.increment();
        }

        I_PREDICTION_MARKET_CONTRACT.trackProgress(
            I_SELF_ID,
            _msgSender(),
            0,
            int256(finalAmount)
        );
        emit BuyOrder(_msgSender(), 0, finalAmount);
    }

    /// @notice SAME AS ABOVE BUT TO PURCHASE 'Yes' TOKENS.
    function buyYesToken(uint256 _amount) external override isOpen {
        if (_amount < 1) revert();

        uint256 owedAmount = (_amount * I_BASE_PRICE) / I_DECIMALS;

        if (I_USDC_CONTRACT.allowance(_msgSender(), address(this)) < owedAmount)
            revert PM_InsufficientApprovedAmount();
        bool success = I_USDC_CONTRACT.transferFrom(
            _msgSender(),
            address(this),
            owedAmount
        );
        if (!success) revert PM_TokenTransferFailed();

        uint256 fee = getFee(owedAmount);
        I_USDC_CONTRACT.transfer(I_VAULT_ADDRESS, fee);

        reserveFEE += fee;
        reserveUSDC += owedAmount - fee;
        reserveYes += owedAmount - fee;

        uint256 finalAmount = _amount - getFee(_amount);
        YesBalances[_msgSender()] += finalAmount;

        if (yesTokenAddressToIndex[_msgSender()] == 0) {
            uint256 index = yesIndex.current();

            yesTokenAddressToIndex[_msgSender()] = index;
            yesHolders.push(_msgSender());

            yesIndex.increment();
        }

        I_PREDICTION_MARKET_CONTRACT.trackProgress(
            I_SELF_ID,
            _msgSender(),
            int256(finalAmount),
            0
        );
        emit BuyOrder(_msgSender(), finalAmount, 0);
    }

    /// @notice Function that allows a trader to dump their tokens.
    /// @param _amount The amount of 'No' Tokens the trader is willing to dump.
    function sellNoToken(uint256 _amount) external override isOpen {
        uint256 totalAmount = (_amount * I_BASE_PRICE) / I_DECIMALS;

        if (NoBalances[_msgSender()] < _amount) revert PM_InvalidAmountSet();

        uint256 fee = getFee(totalAmount);
        I_USDC_CONTRACT.transfer(I_VAULT_ADDRESS, fee);
        reserveFEE += fee;

        uint256 toSend = totalAmount - fee;
        NoBalances[_msgSender()] -= _amount;

        if (NoBalances[_msgSender()] == 0) {
            uint256 index = noTokenAddressToIndex[_msgSender()];

            noHolders[index] = address(0);
            noTokenAddressToIndex[_msgSender()] = 0;
        }

        bool success = I_USDC_CONTRACT.transfer(_msgSender(), toSend);
        if (!success) revert PM_TokenTransferFailed();

        reserveUSDC -= totalAmount;

        I_PREDICTION_MARKET_CONTRACT.trackProgress(
            I_SELF_ID,
            _msgSender(),
            0,
            -1 * int256(_amount)
        );
        emit SellOrder(_msgSender(), 0, _amount);
    }

    /// @notice SAME AS ABOVE BUT FOR 'Yes' TOKENS.
    function sellYesToken(uint256 _amount) external override isOpen {
        uint256 totalAmount = (_amount * I_BASE_PRICE) / I_DECIMALS;

        if (YesBalances[_msgSender()] < _amount) revert PM_InvalidAmountSet();

        uint256 fee = getFee(totalAmount);
        I_USDC_CONTRACT.transfer(I_VAULT_ADDRESS, fee);
        reserveFEE += fee;

        uint256 toSend = totalAmount - fee;
        YesBalances[_msgSender()] -= _amount;

        if (YesBalances[_msgSender()] == 0) {
            uint256 index = yesTokenAddressToIndex[_msgSender()];

            yesHolders[index] = address(0);
            yesTokenAddressToIndex[_msgSender()] = 0;
        }

        bool success = I_USDC_CONTRACT.transfer(_msgSender(), toSend);
        if (!success) revert PM_TokenTransferFailed();

        reserveUSDC -= totalAmount;

        I_PREDICTION_MARKET_CONTRACT.trackProgress(
            I_SELF_ID,
            _msgSender(),
            -1 * int256(_amount),
            0
        );
        emit SellOrder(_msgSender(), _amount, 0);
    }

    /// @notice The trading contract call this function for each individual prediction.
    /// Owner being the trading contract.
    /// @param vote The nature of the winning side.
    /// vote - True => Yes won
    /// vote - False => No won
    function concludePrediction_3(
        bool vote
    ) external override isClosed onlyOwner {
        winner = vote;
        emit WinnerDeclared(vote);

        RewardsClaimable = true;

        //// IGNORE THIS.
        // All the collected fee for the current prediction is sent back to the vault.
        // I_USDC_CONTRACT.transfer(I_VAULT_ADDRESS, reserveFEE);
    }

    /// @notice The function each winner can call to get their share of the total pool.
    /// @notice Based on  how much was the initial pool of winner token and the final pool
    /// @notice being the sum of both the winner and losing side. The final cut of the user is based on the
    /// @notice amount of tokens they held of the winning side.
    function collectRewards() external isClaimable {
        if (rewardCollected[_msgSender()]) revert PM_RewardAlreadyCollected();

        uint256 finalPool = reserveUSDC;
        uint256 initialPool;
        uint256 userTokenCount;
        uint256 userShare;

        if (winner == true) {
            if (YesBalances[_msgSender()] == 0) revert PM_UserDidNotWin();

            initialPool = reserveYes;
            userTokenCount = YesBalances[_msgSender()];
            YesBalances[_msgSender()] = 0;
        } else {
            if (NoBalances[_msgSender()] == 0) revert PM_UserDidNotWin();

            initialPool = reserveNo;
            userTokenCount = NoBalances[_msgSender()];
            NoBalances[_msgSender()] = 0;
        }

        // Calculate the final proportion of the pool they are rewarded.
        userShare = (userTokenCount * finalPool) / initialPool;

        rewardCollected[_msgSender()] = true;

        I_USDC_CONTRACT.transfer(_msgSender(), userShare);
        emit RewardCollected(_msgSender(), userShare);
    }

    /// GETTER FUNCTIONS ==========================================

    function getFee(uint256 _amount) public view returns (uint256) {
        return (_amount * I_FEE) / I_DECIMALS;
    }

    function getNoReserve() external view returns (uint256) {
        return reserveNo;
    }

    function getYesReserve() external view returns (uint256) {
        return reserveYes;
    }

    function getYesTokenCount(address _add) external view returns (uint256) {
        return YesBalances[_add];
    }

    function getNoTokenCount(address _add) external view returns (uint256) {
        return NoBalances[_add];
    }

    receive() external payable {}

    fallback() external payable {}
}
