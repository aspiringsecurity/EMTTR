// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.9;

import "./HackathonManager.sol";

contract HackathonManagerFactory {

    address public Owner;
    uint256 public deploymentFee;
    mapping (string => HackathonManager) public deployedHackathonManagerContractMapping;
    string[] public hackathonNames;

    event HackCreated(address _contractAddress);

    modifier HackNameShouldBeUnique(string memory name){
        require(address(deployedHackathonManagerContractMapping[name]) == address(0), "Hackathon name exists!");
        _;
    }

    constructor(){
        Owner = msg.sender;
    }

    function setFee(uint256 fee) public returns(uint256 _fee){
        require(msg.sender == Owner, "Not owner!");
        deploymentFee = fee;
        _fee = deploymentFee;
    }

    function createNewHack(string memory _name) external payable HackNameShouldBeUnique(_name) returns(address _address){
        require(msg.value >= deploymentFee);
        HackathonManager newDeployed = new HackathonManager(msg.sender, _name);
        deployedHackathonManagerContractMapping[_name] = newDeployed;
        hackathonNames.push(_name);
        _address = address(newDeployed);
        emit HackCreated(_address);
    }

    function getHackContractAddress(string memory _name) public view returns(address _hackContractAddress){
        _hackContractAddress = address(deployedHackathonManagerContractMapping[_name]);
    } 

    function hackathonLength() public view returns(uint256) {
        return hackathonNames.length;
    }
}