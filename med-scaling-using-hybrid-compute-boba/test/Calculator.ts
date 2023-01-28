import { Contract, ContractFactory, providers, utils, Wallet } from "ethers";
import hre, { ethers } from "hardhat";
import chai, { expect } from "chai";
import { solidity } from "ethereum-waffle";
import * as request from "request-promise-native";
import Calculator from "../artifacts/contracts/Calculator.sol/Calculator.json";
import TuringHelperJson from "../artifacts/contracts/TuringHelper.sol/TuringHelper.json";
import L2GovernanceERC20Json from "../abi/L2GovernanceERC20.json";
import BobaTuringCreditJson from "../abi/BobaTuringCredit.json";

chai.use(solidity);
const abiDecoder = require("web3-eth-abi");

const fetch = require("node-fetch");

const cfg = hre.network.config;
const hPort = 1235; // Port for local HTTP server
let urlStr;

const gasOverride = { gasLimit: 11_000_000 };
const local_provider = new providers.JsonRpcProvider(cfg["url"]);

const deployerPK = hre.network.config.accounts[0];
const deployerWallet = new Wallet(deployerPK, local_provider);

let BOBAL2Address;
let BobaTuringCreditAddress;

let Factory__BobaTuringCredit: ContractFactory;
let Factory__CalcContract: ContractFactory;
let calcContract: Contract;
let Factory__TuringHelper: ContractFactory;
let hcHelper: Contract;
let turingCredit: Contract;
let L2BOBAToken: Contract;
let addressesBOBA;
const depositAmount = utils.parseEther("0.10"); // for TuringCredit!

// convenience method for readability
const oldConsole = console.log
console.log = (message?: any, ...optionalParams: any[]) => oldConsole(`--> ${message}`, optionalParams)

const usePrevious = false // TODO: Set to false

describe("Calculator", function() {
  before(async () => {

    if (hre.network.name === "boba_rinkeby") {
      BOBAL2Address = "0xF5B97a4860c1D81A1e915C40EcCB5E4a5E6b8309";
      BobaTuringCreditAddress = "0x208c3CE906cd85362bd29467819d3AcbE5FC1614";
    } else if (hre.network.name === "boba_goerli") {
      BOBAL2Address = "0x4200000000000000000000000000000000000023";
      BobaTuringCreditAddress = "0x4200000000000000000000000000000000000020";
    } else if (hre.network.name === "boba_mainnet") {
      BOBAL2Address = "0x_________________";
      BobaTuringCreditAddress = "0x___________________";
    } else {
      const result = await request.get({
        uri: "http://127.0.0.1:8080/boba-addr.json"
      });
      addressesBOBA = JSON.parse(result);
      BOBAL2Address = addressesBOBA.TOKENS.BOBA.L2;
      BobaTuringCreditAddress = addressesBOBA.BobaTuringCredit;
    }

    Factory__TuringHelper = new ContractFactory(
        TuringHelperJson.abi,
        TuringHelperJson.bytecode,
        deployerWallet
    );

    if (usePrevious) {
      hcHelper = Factory__TuringHelper.attach("0x7CC747D987a761aF6455A60f4F03f02095f74523")
    } else {
      hcHelper = await Factory__TuringHelper.deploy(gasOverride);
    }
    console.log("Helper contract deployed as", hcHelper.address);

    Factory__CalcContract = new ContractFactory(
        Calculator.abi,
        Calculator.bytecode,
        deployerWallet
    );

    if (usePrevious) {
      calcContract = Factory__CalcContract.attach("0xbE778091Ef38E70Ae9918a8C8b229E471580e8a1")
    } else {
      calcContract = await Factory__CalcContract.deploy(
          hcHelper.address,
          "https://q8v29dik8g.execute-api.us-east-1.amazonaws.com/Prod/",
          gasOverride
      );
    }

    console.log("Calculator contract deployed on", calcContract.address);


    L2BOBAToken = new Contract(
        BOBAL2Address,
        L2GovernanceERC20Json.abi,
        deployerWallet
    );

    const tr1 = await hcHelper.addPermittedCaller(calcContract.address);
    const res1 = await tr1.wait();
    console.log("addingPermittedCaller to TuringHelper", res1.events[0].data);


    // prepare to register/fund your Turing Helper
    Factory__BobaTuringCredit = new ContractFactory(
        BobaTuringCreditJson.abi,
        BobaTuringCreditJson.bytecode,
        deployerWallet
    );

    turingCredit = await Factory__BobaTuringCredit.attach(
        BobaTuringCreditAddress
    );
  });

  it("contract should be whitelisted", async () => {
    const tr2 = await hcHelper.checkPermittedCaller(
        calcContract.address,
        gasOverride
    );
    const res2 = await tr2.wait();
    const rawData = res2.events[0].data;
    const result = parseInt(rawData.slice(-64), 16);
    expect(result).to.equal(1);
    console.log(
        "    Test contract whitelisted in TuringHelper (1 = yes)?",
        result
    );
  });

  it("Should register and fund your Turing helper contract in turingCredit", async () => {

    const approveTx = await L2BOBAToken.approve(
        turingCredit.address,
        depositAmount
    );
    await approveTx.wait();

    const depositTx = await turingCredit.addBalanceTo(
        depositAmount,
        hcHelper.address
    );
    await depositTx.wait();
  });

  it("should return the helper address", async () => {
    const helperAddress = await calcContract.hcHelper();
    expect(helperAddress).to.equal(hcHelper.address);
  });

  it('Calculate timeDilation', async () => {
    const properTime = 15
    const velocity = 299792 // almost light speed
    const expectedResult = 8_581_318_860_107_011

    await calcContract.estimateGas.calcTimeDilation(properTime, velocity, gasOverride)
    const tx = calcContract.calcTimeDilation(properTime, velocity, gasOverride)
    await expect(tx).to.emit(calcContract, "CalcResult").withArgs(expectedResult)

    const newTime = expectedResult / 1_000_000_000_000
    const timeDilation = newTime - properTime
    console.log(`Time dilation for properTime: ${timeDilation}`)
  })
});


