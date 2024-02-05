const { ethers } = require("hardhat");
const { expect } = require("chai");

describe("Trading Contract", function () {
  let TradingContract, MockUSDC, mockUSDC;
  let PM_vault, vault;
  let tradingContract;
  let PM_Settlement, settlementContract;
  let MarketHandler, marketHandlerAddress;
  let MockDapiProxy, mockDapi;
  let owner, addr1, addr2;

  beforeEach(async function () {
    MockUSDC = await ethers.getContractFactory("MockUSDC");
    PM_vault = await ethers.getContractFactory("PM_Vault");
    TradingContract = await ethers.getContractFactory("PredictionMarket");
    PM_Settlement = await ethers.getContractFactory("PM_Settlement");
    MockDapiProxy = await ethers.getContractFactory("MockDapiProxy");
    MarketHandler = await ethers.getContractFactory("PM_MarketHandler");
    [owner, addr1, addr2] = await ethers.getSigners();

    mockUSDC = await MockUSDC.deploy();
    await mockUSDC.deployed();

    vault = await PM_vault.deploy(mockUSDC.address);
    await vault.deployed();

    tradingContract = await TradingContract.deploy(mockUSDC.address);
    await tradingContract.deployed();

    settlementContract = await PM_Settlement.deploy(tradingContract.address);
    await settlementContract.deployed();

    mockDapi = await MockDapiProxy.deploy();
    await mockDapi.deployed();
  });

  describe("Deployment", function () {
    it("Should set the right usdc address in trading constructor", async function () {
      expect(await tradingContract.USDCAddress()).to.equal(mockUSDC.address);
      /// Set settlement address
      await tradingContract.setSettlementAddress(settlementContract.address);
      /// Set vault address
      await tradingContract.setVaultAddress(vault.address);
    });
  });

  /// Later set tests for correct addresses and stuff
  describe("Functionality", function () {
    it("Should make a prediction market", async function () {
      /// Mint mockUSDC to owner
      await mockUSDC.connect(owner).mint(owner.address, "1000000000");
      await mockUSDC
        .connect(owner)
        .approve(tradingContract.address, "1000000000");
      /// 100 USDC base price
      /// make a market
      const makeMarket = tradingContract
        .connect(owner)
        .createPrediction(
          "ETH",
          mockDapi.address,
          true,
          "1900000000000000000000",
          50,
          1688702119,
          "100000000",
          owner.address
        );
      await makeMarket;
      // const sleep = ms => new Promise(res => setTimeout(res, ms));
      // await sleep(5000)
      expect();
    });
  });

  describe("Load MarketHandler", function () {
    it("Should load the market handler correctly", async function () {
      marketHandlerAddress = await tradingContract.predictionIdToProxy[1];
      /// Load marketHandler
      const marketHandler = new ethers.Contract(
        marketHandlerAddress,
        MarketHandler.abi
      );
      /// Mint mockUSDC to addr1
      await mockUSDC.connect(addr1).mint(addr1.address, "1000000000");
      await mockUSDC
        .connect(addr1)
        .approve(tradingContract.address, "1000000000");
      /// Buy yes share token (token will go above 1900), buying 2 shares
      await marketHandler.buyYesToken(1);
      expect();
    });
  });
});
