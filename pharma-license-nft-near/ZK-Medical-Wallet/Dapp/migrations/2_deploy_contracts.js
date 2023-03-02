var Election = artifacts.require("Election");
var VoterData = artifacts.require("VoterData");

module.exports = function(deployer) {
  deployer.deploy(Election);
  deployer.deploy(VoterData);
};
