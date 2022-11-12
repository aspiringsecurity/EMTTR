var ecosystem = artifacts.require("./Ecosystem.sol");
var token = artifacts.require("./Token.sol");

module.exports = function(deployer) {
  deployer.deploy(ecosystem,"HEATH","HBAR",16,1000000000);
};
