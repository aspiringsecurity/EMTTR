// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "../v1/MarketHandler.sol";
import "@openzeppelin/contracts/utils/Context.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/utils/Counters.sol";

/// @notice Structure to represent a given Prediction.
struct Prediction {
    string tokenSymbol; // The token symbol in question
    int224 targetPricePoint; // The target price point
    bool isAbove; // This boolean is responsible for defining if the prediction is below or above the price point
    address proxyAddress; // Address of the relevant proxy contract for each asset.
    uint256 fee; // 1 = 0.01%, 100 = 1%, Creator's cut which is further divided as a 20:80 ratio where 20% goes to the protcol and remaining is held by the prediction creator.
    uint256 timestamp; // Timestamp of the creation of prediction
    uint256 deadline; // Timestamp when the prediction is to end
    bool isActive; // Check if the prediction is open or closed
    address marketHandler; // The contract responsible for betting on the prediction.
    uint256 predictionTokenPrice; // The price of either of the token for a given market handler.
    int224 priceAtConclude; // Price returns from the dapi call.
}

/// @notice Error codes
error PM_Conclude_Failed();

/// @notice The centre point of Settlement and each new Market Handler
contract PredictionMarket is Context, Ownable {
    /// @notice Counter to track each new prediction
    using Counters for Counters.Counter;
    Counters.Counter private nextPredictionId;

    /// @notice To avoid DDOS by adding some cost to the creation. Can't be changed once defined.
    uint256 public constant PLATFORM_FEE = 50 * 10 ** 6;

    /// @notice 0.01% * 50 = 0.5%.
    uint256 public TRADING_FEE = 50;

    /// @notice Mapping to track each Prediction with a unique Id.
    mapping(uint256 => Prediction) private predictions;
    /// @notice Mapping to track each Prediction's API3 dAPI proxy address. Only set in a function available
    /// to the owner to restrict any other address from creating a pseudo prediction and manipulate it how they see fit.
    mapping(uint256 => address) private predictionIdToProxy;

    /// @notice To blacklist a certain address and disable their market creation feature.
    mapping(address => bool) private blacklisted;

    /// @notice Event to declare a prediction market is available to be traded.
    /// @param predictionId The unique identifier of the prediction.
    /// @param marketHandler The address of the MarketHandler that enables the prediction to be traded upon.
    /// @param creator The creator responsible for creating the prediction.
    /// @param timestamp The timestamp when the prediction was created to be traded upon.
    event PredictionCreated(
        uint256 indexed predictionId,
        address indexed marketHandler,
        address creator,
        uint256 timestamp,
        uint256 indexed deadline
    );

    /// @dev WILL ADD A BACKUP FORCE_CONCLUDE() TO MAKE SURE IF BECAUSE OF SOME ERROR A CERTAIN PREDICTION WASN'T ABLE
    /// @dev TO BE CONCLUDED EVEN AFTER ALL CONDITIONS PASS THE OWNER WILL STILL BE ABLE TO FORCE THE PREDICTION TO BE
    /// @dev CONCLUDED AND ALLOW THE PARTICIPANTS TO WITHDRAW THEIR REWARDS.

    /// @notice To track if for some reason a certain prediction was not able to be concluded.
    /// @param predictionId The unique identifier of the prediction.
    /// @param isAbove The target orice was supposed to be above a set limit.
    /// @param timestamp The timestamp when conclude failed.
    /// @param priceReading The current price reading provided by a dAPI.
    /// @param priceTarget The target point that was the base for a prediction.
    event ConcludeFatalError(
        uint256 indexed predictionId,
        uint256 timestamp,
        bool isAbove,
        int224 indexed priceReading,
        int224 indexed priceTarget
    );

    event HandlerProgress(
        uint256 indexed predictionId,
        address indexed marketHandler,
        address indexed trader,
        int256 amountYes,
        int256 amountNo
    );

    event PredictionConcluded(uint256 indexed predictionId, uint256 timestamp);

    /// @notice The payment token interface
    IERC20 immutable I_USDC_CONTRACT;

    /// @notice The address that starts the chain of concluding a prediction.
    address public settlementAddress;
    /// @notice The address responsible for storing the funds collected.
    address public vaultAddress;

    /// @notice Check if the address calling the function is the settlementAddress or not
    modifier callerIsSettlement(address _caller) {
        require(_caller == settlementAddress);
        _;
    }

    modifier callerIsMarketHandler(uint256 _id, address _caller) {
        address marketHandlerAddress = predictions[_id].marketHandler;
        if (marketHandlerAddress == address(0))
            revert("Invalid Prediction Id!");
        if (marketHandlerAddress != _caller)
            revert("Caller is not the market handler.");
        _;
    }

    /// @param _usdc The payment token addresTRADING_FEEs.
    constructor(address _usdc) {
        I_USDC_CONTRACT = IERC20(_usdc);

        nextPredictionId.increment();
    }

    function bytes32ToString(
        bytes32 _bytes32Data
    ) public pure returns (string memory) {
        bytes memory bytesData = new bytes(32);
        for (uint i = 0; i < 32; i++) {
            bytesData[i] = _bytes32Data[i];
        }
        return string(bytesData);
    }

    /// @notice Called by the owner on behalf of the _caller and create a market for them.
    /// @notice Step necessary to make sure all the parameters are vaild and are true with no manipulation.
    /// @param _tokenSymbol The symbol to represent the asset we are prediction upon. Eg : BTC / ETH / XRP etc.
    /// @param _isAbove True if for a prediction the price will go above a set limit and false if otherwise.
    /// @param _deadline The timestamp when the target and current price are to be checked against.
    /// @param _basePrice The minimum cost of one 'Yes' or 'No' token for the prediction market to be created.
    /// Is a multiple of 0.01 USD or 1 cent.
    function createPrediction(
        bytes32 _tokenSymbol,
        address _proxyAddress,
        bool _isAbove,
        int224 _targetPricePoint,
        uint256 _deadline,
        uint256 _basePrice
    ) external returns (uint256) {
        /// @param _caller The address that is responsible for paying the platform a set fee and create a new prediction
        /// people can bet upon.
        address _caller = _msgSender();

        require(
            I_USDC_CONTRACT.allowance(_caller, address(this)) >= PLATFORM_FEE,
            "Allowance not set!"
        );
        require(
            _proxyAddress != address(0),
            "Can't have address zero as the proxy's address."
        );
        require(_deadline > block.timestamp, "Deadline can't be in the past.");

        uint256 predictionId = nextPredictionId.current();
        Prediction memory prediction = predictions[predictionId];

        require(prediction.timestamp == 0, "Prediction already exists.");

        bool success = I_USDC_CONTRACT.transferFrom(
            _caller,
            address(this),
            PLATFORM_FEE
        );
        if (!success) revert PM_InsufficientApprovedAmount();

        PM_MarketHandler predictionMH = new PM_MarketHandler(
            predictionId,
            TRADING_FEE,
            _deadline,
            _basePrice,
            address(I_USDC_CONTRACT),
            vaultAddress
        );

        Prediction memory toAdd = Prediction({
            tokenSymbol: bytes32ToString(_tokenSymbol),
            targetPricePoint: _targetPricePoint,
            isAbove: _isAbove,
            proxyAddress: _proxyAddress,
            fee: TRADING_FEE,
            timestamp: block.timestamp,
            deadline: _deadline,
            marketHandler: address(predictionMH),
            predictionTokenPrice: _basePrice,
            isActive: true,
            priceAtConclude: -1
        });

        predictions[predictionId] = toAdd;
        predictionIdToProxy[predictionId] = _proxyAddress;

        nextPredictionId.increment();

        emit PredictionCreated(
            predictionId,
            address(predictionMH),
            _caller,
            block.timestamp,
            _deadline
        );
        return predictionId;
    }

    /// @notice Called by the Settlement contract which concludes the prediction and returns the vote i.e if the
    /// prediction was in the favour of 'Yes' or 'No'.
    /// @param _predictionId The unique identifier for each prediction created.
    /// @param _vote The final result of the prediction.
    /// vote - True : The target price was predicted to be BELOW/ABOVE a threshold AND IS BELOW/ABOVE the threshold respectively.
    /// vote - False : The target price was predicted to be BELOW/ABOVE a threshold BUT IS ABOVE/BELOW the threshold respectively.
    function concludePrediction_2(
        uint256 _predictionId,
        bool _vote,
        address _initiator,
        int224 _readPrice
    ) external callerIsSettlement(_msgSender()) {
        require(
            predictions[_predictionId].deadline <= block.timestamp,
            "Conclude_2 Failed."
        );

        address associatedMHAddress = predictions[_predictionId].marketHandler;
        IMarketHandler mhInstance = IMarketHandler(associatedMHAddress);

        mhInstance.concludePrediction_3(_vote);

        Prediction storage current = predictions[_predictionId];
        current.isActive = false;
        current.priceAtConclude = _readPrice;

        /// Rewards for concluder
        I_USDC_CONTRACT.transfer(_initiator, 40000000);
        I_USDC_CONTRACT.transfer(vaultAddress, 10000000);

        emit PredictionConcluded(_predictionId, block.timestamp);
    }

    /// @notice Setter functions ------
    function setSettlementAddress(address _settlement) external onlyOwner {
        settlementAddress = _settlement;
    }

    function setVaultAddress(address _vault) external onlyOwner {
        vaultAddress = _vault;
    }

    function setTradingFee(uint256 _newFee) external onlyOwner {
        TRADING_FEE = _newFee;
    }

    /// @notice Getter functions ------

    function getNextPredictionId() external view returns (uint256) {
        return nextPredictionId.current();
    }

    function getPrediction(
        uint256 _predictionId
    ) external view returns (Prediction memory) {
        return predictions[_predictionId];
    }

    function getPredictions(
        uint256[] memory _ids,
        uint256 _limit
    ) external view returns (Prediction[] memory) {
        Prediction[] memory toReturn;
        for (uint256 i = 0; i < _limit; i++) {
            toReturn[i] = (predictions[_ids[i]]);
        }
        return toReturn;
    }

    function getProxyForPrediction(
        uint256 _predictionId
    ) external view returns (address) {
        return predictionIdToProxy[_predictionId];
    }

    function getProxiesForPredictions(
        uint256[] memory _ids,
        uint256 _limit
    ) external view returns (address[] memory) {
        address[] memory toReturn;
        for (uint256 i = 0; i < _limit; i++) {
            toReturn[i] = predictionIdToProxy[_ids[i]];
        }
        return toReturn;
    }

    /// SPECIAL FUNCTION ====================================================
    ///
    ///
    /// @notice Function provided to act as an aggregator and help track all the things that are happening on
    /// all of its child market handlers.
    /// @dev Is important since its harder to track each market handler on 'The Graph' separately.
    function trackProgress(
        uint256 _id,
        address _caller,
        int256 _amountYes,
        int256 _amountNo
    ) external callerIsMarketHandler(_id, _msgSender()) {
        address marketHandlerAddress = predictions[_id].marketHandler;
        emit HandlerProgress(
            _id,
            marketHandlerAddress,
            _caller,
            _amountYes,
            _amountNo
        );
    }

    /// ============

    receive() external payable {}

    fallback() external payable {}
}
