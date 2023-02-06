import hre, { artifacts, ethers, upgrades } from "hardhat";
import { Contract, ContractFactory, providers, utils, Wallet } from "ethers";
import TuringHelperJson from "../artifacts/contracts/TuringHelper.sol/TuringHelper.json";
import { getContractFactory } from "@eth-optimism/contracts";

const cfg = hre.network.config;
import L2GovernanceERC20Json from "../abi/L2GovernanceERC20.json";

async function main() {
  const local_provider = new providers.JsonRpcProvider(cfg["url"]);
  const testPrivateKey = process.env.PRIVATE_KEY ?? "0x___________";
  const testWallet = new Wallet(testPrivateKey, local_provider);

  const Factory__Helper = new ContractFactory(
    TuringHelperJson.abi,
    TuringHelperJson.bytecode, testWallet);

  const helper = await Factory__Helper.deploy();
  console.log("Turing Helper contract deployed at", helper.address);

  const SBTFactory = await ethers.getContractFactory("UniversityNFT");
  const sbtContract = await upgrades.deployProxy(SBTFactory, [
      "localhost",
      helper.address
    ],
    {});
  await sbtContract.deployed();
  const implementationSBTAddress = await upgrades.erc1967.getImplementationAddress(sbtContract.address)

  console.log("SBT proxy deployed at", sbtContract.address, "\nImplementation at: ", implementationSBTAddress);

  // white list your ERC721 contract in your helper
  // this is for your own security, so that only your contract can call your helper
  const tr1 = await helper.addPermittedCaller(sbtContract.address);
  const res1 = await tr1.wait();
  console.log("adding your ERC721 as PermittedCaller to TuringHelper", res1.events[0].data);

  /*const helper = new ContractFactory(
    TuringHelperJson.abi,
    TuringHelperJson.bytecode,
    testWallet
  ).attach('0x1EC9BA5f9695E7Bcb5d0a170a3D40D6b9039AfC9')*/

  const useTestnet: boolean = true;

  // use proxy address
  const BobaTuringCreditAddress = useTestnet ? "0x208c3CE906cd85362bd29467819d3AcbE5FC1614" : "0xF8D2f1b0292C0Eeef80D8F47661A9DaCDB4b23bf";
  const turingCredit = getContractFactory(
    "BobaTuringCredit",
    testWallet
  ).attach(BobaTuringCreditAddress);

  const BOBAL2Address = useTestnet ? "0xF5B97a4860c1D81A1e915C40EcCB5E4a5E6b8309" : "0xa18bf3994c0cc6e3b63ac420308e5383f53120d7";
  const L2BOBAToken = new Contract(
    BOBAL2Address,
    L2GovernanceERC20Json.abi,
    testWallet
  );

  const depositAmount = utils.parseEther("0.2");

  // const bobaBalance = await L2BOBAToken.balanceOf(testWallet.address)
  // console.log("BOBA Balance in your account", bobaBalance.toString())

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

  console.log("Boba deposited..");

  const postBalance = await turingCredit.prepaidBalance(
    helper.address
  );
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
