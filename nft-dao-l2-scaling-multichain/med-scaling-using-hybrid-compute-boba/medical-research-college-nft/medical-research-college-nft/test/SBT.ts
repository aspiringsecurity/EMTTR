import {Contract, ContractFactory, providers, Wallet, utils} from 'ethers'
import chai, { expect } from 'chai'
import { solidity } from 'ethereum-waffle'
chai.use(solidity)
import * as request from 'request-promise-native'
import hre, { ethers, upgrades } from "hardhat";
import SBTJson from "../artifacts/contracts/UniversityNFT.sol/UniversityNFT.json"
import TuringHelperJson from "../artifacts/contracts/TuringHelper.sol/TuringHelper.json"
import L2GovernanceERC20Json from '../abi/L2GovernanceERC20.json'
import BobaTuringCreditJson from '../abi/BobaTuringCredit.json'
import * as process from "process";

const cfg = hre.network.config

const PYTHON_BIN = 'python3'
const USE_LOCAL_BACKEND = true
const hPort = 1235 // Port for local HTTP server
let localTuringUrl;
const gasOverride =  { gasLimit: 3_000_000 }

let Factory__Helper: ContractFactory

let sbtContract: Contract
let sbtContractNonOwner: Contract
let sbtContractNonOwner2: Contract
let helper: Contract
let turingCredit: Contract
let L2BOBAToken: Contract
let addressesBOBA

const local_provider = new providers.JsonRpcProvider(cfg['url'])
const local_provider_l1 = new providers.JsonRpcProvider(cfg['url_l1'])

let BOBAL2Address;
let BobaTuringCreditAddress;
const testPrivateKey = hre.network.name === 'boba_local' ? process.env.LOCAL_PRIVATE_KEY : process.env.PRIVATE_KEY ?? '0x___________'
const testPrivateKeyNonOwner = hre.network.name === 'boba_local' ? process.env.LOCAL_PRIVATE_KEY_2 : process.env.PRIVATE_KEY_2 ?? '0x___________'
const testPrivateKeyNonOwner2 = hre.network.name === 'boba_local' ? process.env.LOCAL_PRIVATE_KEY_3 : process.env.PRIVATE_KEY_3 ?? '0x____________'
const testWallet = new Wallet(testPrivateKey, local_provider)
const testWalletNonOwner = new Wallet(testPrivateKeyNonOwner, local_provider)
const testWalletNonOwner2 = new Wallet(testPrivateKeyNonOwner2, local_provider)

// convenience method for readability
const oldConsole = console.log
console.log = (message?: any, ...optionalParams: any[]) => oldConsole(`--> ${message}`, optionalParams)

describe("Turing bridgeable NFT Random 256", function () {

  //#region setup
  const loadPythonResult = (params) => {
    // console.log("Loading python script..", params)
    const {spawn, execSync} = require('child_process');
    return new Promise((resolve , reject) => {
      const childPython = spawn(PYTHON_BIN ,['./aws/run-local-server.py', params]);
      var result = '';
      childPython.stdout.on(`data` , (data) => {
        result += data.toString();
        // console.log("Python: got data -> ", result)
      });

      childPython.on('close' , function(code) {
        // console.log("Python: Child process exited with result & code -> ", result, code)
        resolve(result)
      });

      childPython.stderr.on('data', (err) => {
        console.error('Python error stderr: ', err.toString())
      })

      childPython.on('error' , function(err){
        console.error("Python error: ", err)
        reject(err)
      });

    })
  }

  let showNewDegree = false // will be modified in tests
  const createServer = () => {
    var http = require('http')
    var ip = require("ip")

    var server = module.exports = http.createServer(async function (req, res) {

      if (req.headers['content-type'] === 'application/json') {

        var bodyStr = '';

        req.on('data', function (chunk) {
          bodyStr += chunk.toString()
        })

        req.on('end', async function () {

          var jBody = JSON.stringify({body: bodyStr, logs: false, showNewDegree})

          // console.log("Local server request: ", jBody, req.url)
          var result

          if (req.url === "/test") {
            result = (await loadPythonResult(jBody) as string).replace('\r\n', '') // load Python directly, since examples are currently in Python & to have common test-base
          } else {
            throw new Error('Invalid route: '+ req.route)
          }

          // console.log("Returned object: ", result)

          var jResp2 = {
            id: JSON.parse(bodyStr).id,
            jsonrpc: "2.0",
            result: JSON.parse(JSON.parse(result).body).result
          }

          // console.log("Response local: ", JSON.stringify(jResp2))

          res.end(JSON.stringify(jResp2))
          server.emit('success', bodyStr)

        });

      } else {
        console.log("Other request:", req)
        res.writeHead(400, { 'Content-Type': 'text/plain' })
        res.end('Expected content-type: application/json')
      }
    }).listen(hPort)

    // Get a non-localhost IP address of the local machine, as the target for the off-chain request
    const urlBase = "http://" + ip.address() + ":" + hPort
    localTuringUrl = urlBase + "/test"

    console.log("    Created local HTTP server at", localTuringUrl)
  }

  before(async () => {

    if (USE_LOCAL_BACKEND) {
      createServer()
    }

    if (hre.network.name === 'boba_rinkeby') {
      BOBAL2Address = '0xF5B97a4860c1D81A1e915C40EcCB5E4a5E6b8309'
      BobaTuringCreditAddress = '0x208c3CE906cd85362bd29467819d3AcbE5FC1614'
    } else if (hre.network.name === 'boba_mainnet') {
      BOBAL2Address = '0x_________________'
      BobaTuringCreditAddress = '0x___________________'
    } else {
      const result = await request.get({
        uri: 'http://127.0.0.1:8080/boba-addr.json',
      })
      addressesBOBA = JSON.parse(result)
      BOBAL2Address = addressesBOBA.TOKENS.BOBA.L2
      BobaTuringCreditAddress = addressesBOBA.BobaTuringCredit
    }


    // Deploy your Turing Helper
    Factory__Helper = new ContractFactory(
      TuringHelperJson.abi,
      TuringHelperJson.bytecode,
      testWallet)

    helper = await Factory__Helper.deploy()
    console.log("Turing Helper contract deployed at", helper.address)

    const SBTFactory = new ContractFactory(
      SBTJson.abi,
      SBTJson.bytecode,
      testWallet,
    );
    sbtContract = await upgrades.deployProxy(SBTFactory, [
        "http://localhost/",
        USE_LOCAL_BACKEND ? localTuringUrl : "https://4hapc4qo71.execute-api.us-east-1.amazonaws.com/Prod/",
        "https://api.university.edu/img/",
        helper.address
      ],
      {});
    await sbtContract.deployed();
    const implementationSBTAddress = await upgrades.erc1967.getImplementationAddress(sbtContract.address)
    sbtContractNonOwner = sbtContract.connect(testWalletNonOwner)
    sbtContractNonOwner2 = sbtContract.connect(testWalletNonOwner2)

    console.log("SBT proxy deployed at", sbtContract.address, "Implementation at: ", implementationSBTAddress);


    // white list your contract in your helper
    // this is for your own security, so that only your contract can call your helper
    const tr1 = await helper.addPermittedCaller(sbtContract.address)
    const res1 = await tr1.wait()
    console.log("adding your token as PermittedCaller to TuringHelper", res1.events[0].data)

    L2BOBAToken = new Contract(
      BOBAL2Address,
      L2GovernanceERC20Json.abi,
      testWallet
    )

    turingCredit = new ContractFactory(
      BobaTuringCreditJson.abi,
      BobaTuringCreditJson.bytecode,
      testWallet).attach(BobaTuringCreditAddress)

  })
  //#endregion

  it('Should register and fund your Turing helper contract in turingCredit', async () => {

    const depositAmount = utils.parseEther('1')

    const bobaBalance = await L2BOBAToken.balanceOf(testWallet.address)
    console.log("BOBA Balance in your account", bobaBalance.toString())

    const approveTx = await L2BOBAToken.approve(
      turingCredit.address,
      depositAmount
    )
    await approveTx.wait()

    const depositTx = await turingCredit.addBalanceTo(
      depositAmount,
      helper.address
    )
    await depositTx.wait()

    const postBalance = await turingCredit.prepaidBalance(
      helper.address
    )

  })

  it("Your ERC1155 contract should be whitelisted", async () => {
    const tr2 = await helper.checkPermittedCaller(sbtContract.address, gasOverride)
    const res2 = await tr2.wait()
    const rawData = res2.events[0].data
    const result = parseInt(rawData.slice(-64), 16)
    expect(result).to.equal(1)
    console.log("ERC1155 contract whitelisted in TuringHelper (1 = yes)?", result)
  })

  const linkedInUser = 'wsdt'
  const expectedMajorNames = ["Computer Science 2022", "History 1994"]
  const expectedTokenIDs = [1, 2]

  it("Load & revoke degree", async () => {
    for (const expectedTokenID of expectedTokenIDs) {
      const balance = await sbtContractNonOwner.balanceOf(testWalletNonOwner.address, expectedTokenID)
      expect(balance).to.be.equal(0)
    }

    await sbtContractNonOwner.estimateGas.refreshAlumnus(testWallet.address, gasOverride)
    const tr0 = await sbtContractNonOwner.refreshAlumnus(testWallet.address, gasOverride)
    const res0 = await tr0.wait()
    expect(res0).to.be.ok
    for (const expectedTokenID of expectedTokenIDs) {
      const balance = await sbtContractNonOwner.balanceOf(testWallet.address, expectedTokenID)
      expect(balance).to.be.equal(1)
    }


    await sbtContractNonOwner.estimateGas.refreshAlumnus(testWalletNonOwner.address, gasOverride)
    const tr1 = await sbtContractNonOwner.refreshAlumnus(testWalletNonOwner.address, gasOverride)
    const res1 = await tr1.wait()
    expect(res1).to.be.ok

    for (const expectedTokenID of expectedTokenIDs) {
      const balance = await sbtContractNonOwner.balanceOf(testWalletNonOwner.address, expectedTokenID)
      expect(balance).to.be.equal(1)
    }

    showNewDegree = true; // API should now return new degree (just for testing)
    await sbtContractNonOwner.estimateGas.refreshAlumnus(testWalletNonOwner.address, gasOverride)
    const tr2 = await sbtContractNonOwner.refreshAlumnus(testWalletNonOwner.address, gasOverride)
    const res2 = await tr2.wait()
    expect(res2).to.be.ok

    const expectedDegreesWithNewObtained = [...expectedTokenIDs, 3]
    for (const expectedDegree of expectedDegreesWithNewObtained) {
      const balance = await sbtContractNonOwner.balanceOf(testWalletNonOwner.address, expectedDegree)
      expect(balance).to.be.equal(1)
    }

    const newDegreeID = 3
    showNewDegree = false;
    await sbtContract.estimateGas.revokeDegree(testWalletNonOwner.address, [newDegreeID])
    const tr3 = await sbtContract.revokeDegree(testWalletNonOwner.address, [newDegreeID], gasOverride)
    const res3 = await tr3.wait()
    expect(res3).to.be.ok

    for (const expectedTokenID of expectedTokenIDs) {
      const balance = await sbtContractNonOwner.balanceOf(testWalletNonOwner.address, expectedTokenID)
      expect(balance).to.be.equal(1)
    }
    const balanceNewDegreeRevoked = await sbtContractNonOwner.balanceOf(testWalletNonOwner.address, newDegreeID)
    expect(balanceNewDegreeRevoked).to.be.equal(0);
  });

  it('Fail to change wallet as non-owner', async () => {
    await expect(sbtContractNonOwner.changeWallet(testWalletNonOwner.address, testWallet.address)).to.be.revertedWith("Ownable: caller is not the owner");
  })

  it('Fail to change wallet as owner when no NFTs in there', async () => {
    // use random wallet
    await expect(sbtContract.estimateGas.changeWallet("0xE159EfEcF2053f870AE504dF213A40F7E7C457d0", testWalletNonOwner2.address)).to.be.revertedWith("No majors")
  })

  it('Fail to change wallet as owner of own token since other wallet already registered', async () => {
    await expect(sbtContract.estimateGas.changeWallet(testWallet.address, testWalletNonOwner.address)).to.be.revertedWith("To wallet used")
  })

  it('Change wallet as owner of own token', async () => {
    const balanceBefore = await sbtContract.balanceOfBatch(
      [testWallet.address,testWalletNonOwner2.address,testWallet.address,testWalletNonOwner2.address],
      [expectedTokenIDs[0], expectedTokenIDs[0], expectedTokenIDs[1], expectedTokenIDs[1]])
    expect(balanceBefore[0]).to.be.equal(1)
    expect(balanceBefore[1]).to.be.equal(0)
    expect(balanceBefore[2]).to.be.equal(1)
    expect(balanceBefore[3]).to.be.equal(0)
    const studentDataBefore = await sbtContract.alumniData(testWallet.address)
    const studentDataBefore1 = await sbtContract.alumniData(testWalletNonOwner2.address)

    expect(studentDataBefore).to.be.equal(linkedInUser)
    expect(studentDataBefore1).to.be.equal('')

    await sbtContract.estimateGas.changeWallet(testWallet.address, testWalletNonOwner2.address, gasOverride)
    const tx = await sbtContract.changeWallet(testWallet.address, testWalletNonOwner2.address, gasOverride)
    const res = await tx.wait()
    expect(res).to.be.ok

    const balanceAfter = await sbtContract.balanceOfBatch([testWalletNonOwner2.address,testWallet.address,testWalletNonOwner2.address,testWallet.address],
      [expectedTokenIDs[0], expectedTokenIDs[0], expectedTokenIDs[1], expectedTokenIDs[1]])
    expect(balanceAfter[0]).to.be.equal(1)
    expect(balanceAfter[1]).to.be.equal(0)
    expect(balanceAfter[2]).to.be.equal(1)
    expect(balanceAfter[3]).to.be.equal(0)
    const studentDataAfter = await sbtContract.alumniData(testWalletNonOwner2.address)
    const studentDataAfter1 = await sbtContract.alumniData(testWallet.address)
    expect(studentDataAfter).to.be.equal(linkedInUser)
    expect(studentDataAfter1).to.be.equal('')
  })

  it('Change wallet as owner from other student', async () => {
    const balanceBefore = await sbtContract.balanceOfBatch([testWalletNonOwner2.address,testWallet.address,testWalletNonOwner2.address,testWallet.address],
      [expectedTokenIDs[0], expectedTokenIDs[0], expectedTokenIDs[1], expectedTokenIDs[1]])
    expect(balanceBefore[0]).to.be.equal(1)
    expect(balanceBefore[1]).to.be.equal(0)
    expect(balanceBefore[2]).to.be.equal(1)
    expect(balanceBefore[3]).to.be.equal(0)
    const studentDataBefore = await sbtContract.alumniData(testWalletNonOwner2.address)
    const studentDataBefore1 = await sbtContract.alumniData(testWallet.address)
    expect(studentDataBefore).to.be.equal(linkedInUser)
    expect(studentDataBefore1).to.be.equal('')
    await sbtContract.estimateGas.changeWallet(testWalletNonOwner2.address, testWallet.address, gasOverride)
    const tx = await sbtContract.changeWallet(testWalletNonOwner2.address, testWallet.address, gasOverride)
    const res = await tx.wait()
    expect(res).to.be.ok

    const balanceAfter = await sbtContract.balanceOfBatch([testWallet.address,testWalletNonOwner2.address,testWallet.address,testWalletNonOwner2.address],
      [expectedTokenIDs[0], expectedTokenIDs[0], expectedTokenIDs[1], expectedTokenIDs[1]])
    expect(balanceAfter[0]).to.be.equal(1)
    expect(balanceAfter[1]).to.be.equal(0)
    expect(balanceAfter[2]).to.be.equal(1)
    expect(balanceAfter[3]).to.be.equal(0)
    const studentDataAfter = await sbtContract.alumniData(testWallet.address)
    const studentDataAfter1 = await sbtContract.alumniData(testWalletNonOwner2.address)
    expect(studentDataAfter).to.be.equal(linkedInUser)
    expect(studentDataAfter1).to.be.equal('')
  })

  it('Fail to transfer and batchTransfer since non-transferable', async () => {
    const tx1 = sbtContractNonOwner.estimateGas.safeTransferFrom(testWalletNonOwner.address, testWallet.address, expectedTokenIDs[0], 1, ethers.constants.HashZero)
    await expect(tx1).to.be.revertedWith("Non transferable")

    const tx2 = sbtContractNonOwner.estimateGas.safeBatchTransferFrom(testWalletNonOwner.address, testWallet.address, expectedTokenIDs, [1,1], ethers.constants.HashZero)
    await expect(tx2).to.be.revertedWith("Non transferable")
  })

  it('Metadata is valid', async () => {
    let uri = await sbtContract.uri(expectedTokenIDs[0], gasOverride)
    const jsonStr = Buffer.from(uri.substring(uri.indexOf(',')+1), 'base64').toString()
    const decodedMetadata = JSON.parse(jsonStr)
    console.log("Decoded metadata = ", jsonStr)

    expect(decodedMetadata['name']).to.be.not.null;
    expect(decodedMetadata['description']).to.be.not.null;
    expect(decodedMetadata['attributes']).to.be.not.null;
    expect(decodedMetadata['attributes']?.length).to.be.greaterThan(0)
    expect(decodedMetadata['attributes'][0].trait_type).to.be.equal('Computer Science 2022')
    expect(decodedMetadata['attributes'][0].value).to.be.equal('Received')
    expect(decodedMetadata['image']).to.be.not.null;
    expect(decodedMetadata['image']?.length).to.be.greaterThan(2);
  })

  it('Fail when querying metadata of non-existent token', async () => {
    await expect(sbtContract.uri(9862, gasOverride)).to.be.revertedWith("ERC1155: URI get of nonexistent token");
  })
})

