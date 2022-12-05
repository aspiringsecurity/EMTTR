const AgeVerifier = artifacts.require("AgeVerifier");
const PasswordVerifier = artifacts.require("PasswordVerifier");
const VoteVerifier = artifacts.require("VoteVerifier");

module.exports = function(deployer) {
  deployer.deploy(AgeVerifier);
  deployer.deploy(PasswordVerifier);
  deployer.deploy(VoteVerifier);
};
