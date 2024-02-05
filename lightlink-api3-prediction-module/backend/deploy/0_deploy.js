module.exports = async ({ getNamedAccounts, deployments }) => {
  const { deploy } = deployments;
  const { deployer } = await getNamedAccounts();

  const provider = ethers.provider;
  const signer = provider.getSigner(deployer);

  const Mock = await deploy("MockUSDC", {
    from: deployer,
  });
  console.log("\nDeployed MockUSDC at   :", Mock.address);

  const Vault = await deploy("PM_Vault", {
    from: deployer,
    args: [Mock.address],
  });
  console.log("Deployed Vault at      :", Vault.address);

  /// THESE ARE ALL ON POLYGON TESTNET
  const Trading = await deploy("PredictionMarket", {
    from: deployer,
    args: [Mock.address],
  });
  console.log("Deployed Trading at    :", Trading.address);

  const Settlement = await deploy("PM_Settlement", {
    from: deployer,
    args: [Trading.address],
  });
  console.log("Deployed Settlement at :", Settlement.address, "\n");

  const trading = new ethers.Contract(Trading.address, Trading.abi, signer);
  await trading.setSettlementAddress(Settlement.address);
  await trading.setVaultAddress(Vault.address);

  const mock = new ethers.Contract(Mock.address, Mock.abi, signer);
  await mock.mint(deployer, 1000000000000);
};

module.exports.tags = ["All"];