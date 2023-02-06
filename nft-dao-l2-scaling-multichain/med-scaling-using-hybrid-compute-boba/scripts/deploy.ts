import hre, { ethers } from "hardhat";
import {Contract, ContractFactory, providers, utils, Wallet} from "ethers";
import TuringHelperJson from "../artifacts/contracts/TuringHelper.sol/TuringHelper.json";
import L2GovernanceERC20Json from "../abi/L2GovernanceERC20.json";
import BobaTuringCreditJson from "../abi/BobaTuringCredit.json";
import * as request from "request-promise-native";

const cfg = hre.network.config;

const depositAmount = utils.parseEther("0.10"); // for TuringCredit!

async function main() {

  const local_provider = new providers.JsonRpcProvider(cfg["url"]);
  const testPrivateKey = process.env.PRIVATE_KEY ?? "0x___________";
  const testWallet = new Wallet(testPrivateKey, local_provider);
  let BOBAL2Address, BobaTuringCreditAddress, addressesBOBA

  if (hre.network.name === "boba_rinkeby") {
    BOBAL2Address = "0xF5B97a4860c1D81A1e915C40EcCB5E4a5E6b8309";
    BobaTuringCreditAddress = "0x208c3CE906cd85362bd29467819d3AcbE5FC1614";
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

  let L2BOBAToken = new Contract(
      BOBAL2Address,
      L2GovernanceERC20Json.abi,
      testWallet
  );

  // prepare to register/fund your Turing Helper
  const Factory__BobaTuringCredit = new ContractFactory(
      BobaTuringCreditJson.abi,
      BobaTuringCreditJson.bytecode,
      testWallet
  );

  const turingCredit = await Factory__BobaTuringCredit.attach(
      BobaTuringCreditAddress
  );

  const Factory__Helper = new ContractFactory(
      TuringHelperJson.abi,
      TuringHelperJson.bytecode, testWallet);

  const helper = await Factory__Helper.deploy();
  console.log("Turing Helper contract deployed at", helper.address);


  const Factory__Calculator = await ethers.getContractFactory("Calculator");
  const calculatorContract = await Factory__Calculator.deploy(helper.address, "https://q8v29dik8g.execute-api.us-east-1.amazonaws.com/Prod/"); // this is our example endpoint for this workshop
  await calculatorContract.deployed();

  console.log("Calculator deployed at", calculatorContract.address);


  const owner = await helper.owner();
  console.log("OWNER: ", owner)

  const tx = await helper.addPermittedCaller(calculatorContract.address)
  const res = await tx.wait()

  console.log("Have added new contract address", calculatorContract.address, "to helper: ", helper.address)


  const approveTx = await L2BOBAToken.approve(
      turingCredit.address,
      depositAmount
  );
  await approveTx.wait();

  const depositTx = await turingCredit.addBalanceTo(
      depositAmount,
      helper.address
  );
  await depositTx.wait();

  console.log("Added balance to hcHelper")
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});