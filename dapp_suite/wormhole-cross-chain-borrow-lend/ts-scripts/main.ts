import * as ethers from "ethers"
import {
  checkFlag,
  getHub,
  getSpoke,
  getWallet,
  loadDeployedAddresses as getDeployedAddresses,
  wait,
  getArg
} from "./utils"
import { ERC20Mock__factory, } from "./ethers-contracts"
import { deploy } from "./deploy"
import { deployMockToken } from "./deploy-mock-tokens"
import { getStatus } from "./getStatus"
import { ChainName } from "@certusone/wormhole-sdk"

async function main() {
  if (checkFlag("--deployCrossChainBorrowLend")) {
    await deploy()
    return
  }
  if (checkFlag("--deployMockToken")) {
    await deployMockToken()
    return
  }
  if(checkFlag("--getStatus")) {
    const status = await getStatus(getArg(["--chain", "-c", "--sourceChain"]) as ChainName || "celo", getArg(["--txHash", "--tx", "-t"]) || "");
    console.log(status.info);
  }
}

main().catch(e => {
  console.error(e)
  process.exit(1)
})
