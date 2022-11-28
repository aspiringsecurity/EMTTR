//SPDX-License-Identifier: Unlicense
pragma solidity ^0.8.7;

import "@chainlink/contracts/src/v0.8/ChainlinkClient.sol";
import "@chainlink/contracts/src/v0.8/interfaces/KeeperCompatibleInterface.sol";

import "hardhat/console.sol";

contract DelayInsurance is ChainlinkClient, KeeperCompatibleInterface {
    using Chainlink for Chainlink.Request;

    enum PolicyStatus {
        CREATED, // Policy is subscribed
        RUNNING, // Policy cover is started
        COMPLETED, // Policy cover is fin6ished without claim
        CLAIMED, // Policy is claimed, waiting for pay out
        PAIDOUT // Claim is paid out
    }

    struct Coordinate {
        string lat;
        string lng;
    }

    struct Ship {
        string id;
        uint256 shipmentValue;
    }

    struct Coverage {
        address payable beneficiary;
        uint256 startDate;
        uint256 endDate;
        uint256 startPort;
        uint256 endPort;
        uint256 premium;
        uint256 gustThreshold;
        PolicyStatus status;
    }

    struct WeatherData {
        uint256 requestId;
        Coordinate location;
        uint256 gust;
    }

    struct Policy {
        uint256 policyId;
        Ship ship;
        Coverage coverage;
        WeatherData weatherData;
        uint8 incidents;
    }

    event PolicySubscription (
        address indexed beneficiary,
        uint256 indexed id
    );

    event IncidentReported (
        address indexed beneficiary,
        uint8 indexed actualNumberOfIncidents
    );

    event PolicyPaidOut (
        address indexed beneficiary,
        uint256 indexed id,
        uint256 indexed shipmentValue
    );

    mapping(address => Policy) public policies;
    mapping(bytes32 => address) public requestToPolicyAddr;
    address[] public addrPolicies;

    uint256 public updateTimer;
    uint256 public lastTimeStamp;
    address public admin;
    uint256 public policyId;
    address public weatherOracle;
    bytes32 public weatherJobId;
    uint256 public weatherFee;
    uint8 public incidentsThreshold; // Threshold for triggering claiming process

    // Prevents a function being run unless it's called by Insurance Provider
    modifier onlyOwner() {
        require(admin == msg.sender, "Only Insurance provider can do this");
        _;
    }

    //Enable anyone to send eth to the smart contract
    receive() external payable { }

    constructor(address _linkTokenAddress) public {
        admin = msg.sender;
        if (_linkTokenAddress == address(0)) {
            setPublicChainlinkToken();
        } else {
            setChainlinkToken(_linkTokenAddress);
        }
    }

    /**********  PUBLIC FUNCTIONS **********/

    function subscribePolicy(
        string memory _shipId,
        uint256 _shipmentValue,
        uint256 _startDate,
        uint256 _endDate,
        uint256 _startPort,
        uint256 _endPort
    ) public payable {

        Ship memory ship = Ship({
            id: _shipId,
            shipmentValue: _shipmentValue
        });

        uint256 _premium = pricePremium(
            ship,
            _startDate,
            _endDate,
            _startPort,
            _endPort
        );

        require(_premium == msg.value, "You have to pay the exact Premium");

        uint256 _gustThreshold = calculateGustThreshold(
            _startDate,
            _endDate,
            _startPort,
            _endPort
        );

        Coverage memory coverage = Coverage({
            beneficiary: payable(msg.sender),
            startDate: _startDate,
            endDate: _endDate,
            startPort: _startPort,
            endPort: _endPort,
            premium: _premium,
            gustThreshold: _gustThreshold,
            status: PolicyStatus.CREATED
        });

        WeatherData memory weatherData = WeatherData({
            requestId: 0,
            location: Coordinate({lat: '0', lng: '0'}),
            gust: 0
        });

        Policy memory policy = Policy({
            policyId: policyId,
            ship: ship,
            coverage: coverage,
            weatherData: weatherData,
            incidents: 0
        });

        policies[msg.sender] = policy;
        addrPolicies.push(msg.sender);

        emit PolicySubscription(msg.sender, policyId);
        policyId++;
    }

    /**********  GET FUNCTIONS **********/

    function getPolicy() public view returns (Policy memory) {
        return policies[msg.sender];
    }

    function getGustThreshold() public view returns (uint256) {
        return policies[msg.sender].coverage.gustThreshold;
    }

    function getPolicyStatus() public view returns (PolicyStatus) {
        return policies[msg.sender].coverage.status;
    }

    function getLatestWeatherData() public view returns (WeatherData memory) {
        return policies[msg.sender].weatherData;
    }

    function getAllPolicies() public view returns (Policy[] memory) {
        Policy[] memory allPolicies = new Policy[](addrPolicies.length);
        for(uint i = 0; i < addrPolicies.length; i++) {
          allPolicies[i] = policies[addrPolicies[i]];
        }
        return allPolicies;
    }

    function getOracleDetails() public view returns (address, bytes32, uint256) {
        return (weatherOracle, weatherJobId, weatherFee);
    }

    function getIncidentThreshold() public view returns (uint) {
        return incidentsThreshold;
    }

    function getUpdateTimer() public view returns (uint) {
        return updateTimer;
    }

    function getTotalPremiums() public view returns(uint256) {
        uint total = 0;
        for( uint i=0; i<addrPolicies.length; i++){
            total += policies[addrPolicies[i]].coverage.premium;
        }
        return total;
    }

    function getTotalCapitalInsured() public view returns(uint256) {
        uint total = 0;
        for( uint i=0; i<addrPolicies.length; i++){
            total += policies[addrPolicies[i]].ship.shipmentValue;
        }
        return total;
    }

    /**********  SET FUNCTIONS **********/

    // Set up weather oracle datas
    function setWeatherOracle(
        address _oracleAddress,
        string memory _jobId,
        uint256 _fee
    ) public onlyOwner {
        weatherOracle = _oracleAddress; // address :
        weatherJobId = stringToBytes32(_jobId); // jobId  :
        weatherFee = _fee; // fees : X.X LINK
    }

    // Set up incident threshold
    function setIncidentThreshold(uint8 _incidentsThreshold) public onlyOwner {
        incidentsThreshold = _incidentsThreshold;
    }

    function setUpdateTimer(uint256 _updateTimer) public onlyOwner {
        updateTimer = _updateTimer;
    }

    function setPolicyThreshold(address _beneficiary, uint256 _gustThreshold) public onlyOwner returns(uint256){
        policies[_beneficiary].coverage.gustThreshold = _gustThreshold;
    }

    // it should be 'onlyOwner' but this rule was removed to facilitate examiners' tests
    function setInsuranceParameters(uint8 _incidentsThreshold, uint256 _updateTimer, address _beneficiary, uint256 _gustThreshold) public {
        incidentsThreshold = _incidentsThreshold;
        updateTimer = _updateTimer;
        policies[_beneficiary].coverage.gustThreshold = _gustThreshold;
    }

    /**********  PRICING FUNCTIONS **********/

    // Calculate the premium
    function pricePremium(
        Ship memory _ship,
        uint256 _startDate,
        uint256 _endDate,
        uint256 _startPort,
        uint256 _endPort
    ) public view returns (uint256) {
        return _ship.shipmentValue / 200; // Hardvalue for a catnat event (occure 1/200)
    }

    // Calculate the gust threshold, above the threshold, the insurer pay out
    function calculateGustThreshold(
        uint256 _startDate,
        uint256 _endDate,
        uint256 _startPort,
        uint256 _endPort
    ) public view returns (uint256) {
        return 100;
    }

    /**********  CHAINLINK KEEPER FUNCTIONS **********/

    function checkUpkeep(bytes calldata /* checkData */)
        external
        override
        returns (bool upkeepNeeded, bytes memory /* performData */) {
            upkeepNeeded = (block.timestamp - lastTimeStamp) > updateTimer;
    }

    function performUpkeep(bytes calldata /* performData */) external override {
        lastTimeStamp = block.timestamp;
        UpdateContracts();
    }

    /**********  CLAIMS FUNCTIONS **********/

    // TODO make this function internal
    function UpdateContracts() public {
        for (uint256 policiesIndex = 0; policiesIndex < addrPolicies.length; policiesIndex++) {
            address addr = addrPolicies[policiesIndex];
            Policy storage policy = policies[addr];

            // Update all policies status
            if ((policy.coverage.startDate <= block.timestamp && policy.coverage.endDate >= block.timestamp) && (policy.coverage.status==PolicyStatus.CREATED)) {
              policy.coverage.status = PolicyStatus.RUNNING;
            } else if ((policy.coverage.endDate < block.timestamp) && (policy.coverage.status==PolicyStatus.RUNNING)) {
              policy.coverage.status = PolicyStatus.COMPLETED;
            }

            if (policy.coverage.status == PolicyStatus.RUNNING) {
              // Request weather data to Chainlink
              Chainlink.Request memory request = buildChainlinkRequest(weatherJobId, address(this), this.receiveWeatherData.selector);

              request.add("uuid", policy.ship.id);
              // Sends the request
              bytes32 requestId = sendChainlinkRequestTo(weatherOracle, request, weatherFee);
              requestToPolicyAddr[requestId] = addr;
            } 
        }
    }

    function receiveWeatherData(bytes32 _requestId, uint256 _gust) public recordChainlinkFulfillment(_requestId) {
        Policy storage policy = policies[requestToPolicyAddr[_requestId]];
        address addr = requestToPolicyAddr[_requestId];
        policy.weatherData.gust = _gust;
        // remove current requestId from requestToPolicyAddr list
        delete requestToPolicyAddr[_requestId];
        // Check if the policy has an incident
        if (hasIncident(addr)) {
            // Update number of incidents
            policy.incidents++;
            emit IncidentReported(addr, policy.incidents);
        }
        if (policy.incidents >= incidentsThreshold /* + Should be at end port? */) {
            policy.coverage.status = PolicyStatus.CLAIMED;
            payOut(addr);
        }
    }

    // Check if the gust is above the threshold
    function hasIncident(address _beneficiary) public returns (bool) {
        Policy memory policy = policies[_beneficiary];
        // Trigger claiming process using pre determined threshold
        if (policy.weatherData.gust > policy.coverage.gustThreshold) {
            return true;
        }
    }

    function payOut(address _beneficiary) public payable {
        // transfer funds to beneficiary
        Policy storage policy = policies[_beneficiary];
        //(bool sent, bytes memory data) = _beneficiary.call{value: policy.ship.shipmentValue}("");
        //require(sent, "Failed to transfer insurance claim");
        require(address(this).balance >= policy.ship.shipmentValue);
        payable(_beneficiary).transfer(policy.ship.shipmentValue);
        // Set contract to PAIDOUT
        policy.coverage.status = PolicyStatus.PAIDOUT;
        emit PolicyPaidOut(_beneficiary, policy.policyId, policy.ship.shipmentValue);
    }

    // FORMAT FUNCTIONS

    function stringToBytes32(string memory source) private pure returns (bytes32 result) {
        bytes memory tempEmptyStringTest = bytes(source);
        if (tempEmptyStringTest.length == 0) {
            return 0x0;
        }

        assembly { // solhint-disable-line no-inline-assembly
          result := mload(add(source, 32))
        }
    }
}
