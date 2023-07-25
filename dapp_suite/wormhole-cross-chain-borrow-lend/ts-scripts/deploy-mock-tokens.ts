import { ethers } from "ethers"
import { ERC20Mock__factory } from "./ethers-contracts"
import {
  loadDeployedAddresses,
  getWallet,
  wait,
  loadConfig,
  storeDeployedAddresses,
  getChain,
} from "./utils"
import {
  ChainId,
  attestFromEth,
  createWrappedOnEth,
  getSignedVAAWithRetry,
  parseSequenceFromLogEth,
  tryNativeToHexString,
} from "@certusone/wormhole-sdk"
import * as grpcWebNodeHttpTransport from "@improbable-eng/grpc-web-node-http-transport"
import { ChainInfo, getArg } from "./utils"

export async function deployMockToken() {
  const deployed = loadDeployedAddresses()
  const from = getChain(14)

  const signer = getWallet(from.chainId)
  const celoToken = await new ERC20Mock__factory(signer).deploy("CeloToken", "Celo")
  await celoToken.deployed()
  console.log(`CeloTest Token deployed to ${celoToken.address} on chain ${from.chainId}`)
  deployed.erc20s[from.chainId] = [celoToken.address]

  console.log("Minting...")
  await celoToken.mint(signer.address, ethers.utils.parseEther("10")).then(wait)
  console.log("Minted 10 celotest to signer")

  console.log(
    `Attesting tokens with token bridge on chain(s) ${loadConfig()
      .chains.map(c => c.chainId)
      .filter(c => c !== from.chainId)
      .join(", ")}`
  )
  for (const chain of loadConfig().chains) {
    if (chain.chainId === from.chainId) {
      continue
    }
    await attestWorkflow({ from: getChain(from.chainId), to: chain, token: celoToken.address })
  }

  storeDeployedAddresses(deployed)
}

async function attestWorkflow({
  to,
  from,
  token,
}: {
  to: ChainInfo
  from: ChainInfo
  token: string
}) {
  const attestRx: ethers.ContractReceipt = await attestFromEth(
    from.tokenBridge!,
    getWallet(from.chainId),
    token
  )
  const seq = parseSequenceFromLogEth(attestRx, from.wormhole)

  const res = await getSignedVAAWithRetry(
    ["https://api.testnet.wormscan.io"],
    Number(from) as ChainId,
    tryNativeToHexString(from.tokenBridge, "ethereum"),
    seq.toString(),
    { transport: grpcWebNodeHttpTransport.NodeHttpTransport() }
  )
  const createWrappedRx = await createWrappedOnEth(
    to.tokenBridge,
    getWallet(to.chainId),
    res.vaaBytes
  )
  console.log(
    `Attested token from chain ${from.chainId} to chain ${to.chainId}`
  )
}
