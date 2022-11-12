var DrugEcosystem = artifacts.require('./DrugEcosystem.sol')
var TestDrugEcosystem = artifacts.require('./TestDrugEcosystem.sol')

module.exports = function (deployer) {
  deployer.deploy(DrugEcosystem)
  deployer.deploy(TestDrugEcosystem)
}
