var DrugTask = artifacts.require('./DrugTask.sol')
var TestDrugTask = artifacts.require('./TestDrugTask.sol')

module.exports = function (deployer) {
  deployer.deploy(DrugTask)
  deployer.deploy(TestDrugTask)
}
