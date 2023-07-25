import { ethers } from "ethers"
import { ERC20Mock__factory, Hub__factory, Spoke__factory } from "./ethers-contracts"
import {
  loadConfig,
  getWallet,
  storeDeployedAddresses,
  getChain,
  wait,
  loadDeployedAddresses,
} from "./utils"
import {
  tryNativeToUint8Array
} from "@certusone/wormhole-sdk"

export async function deploy() {
  const config = loadConfig()

  // Fuji is hub chain
  // One spoke: celo
  const deployed = loadDeployedAddresses()

  const hubChain = getChain(6);
  const hub = await new Hub__factory(getWallet(6)).deploy(
    hubChain.wormholeRelayer,
    hubChain.tokenBridge!,
    hubChain.wormhole
  );
  await hub.deployed();
  deployed.hub = {
    address: hub.address,
    chainId: 6
  }
  console.log(
    `Hub deployed to ${hub.address} on chain ${6}`
  )

  for (const chainId of [14]) {
    const chain = getChain(chainId)
    const signer = getWallet(chainId)

    const spoke = await new Spoke__factory(signer).deploy(
      6,
      hub.address,
      chain.wormholeRelayer,
      chain.tokenBridge!,
      chain.wormhole
    )
    await spoke.deployed()

    deployed.spokes[chainId] = spoke.address
    console.log(
      `Spoke deployed to ${spoke.address} on chain ${chainId}`
    )

    const tx = await hub.setRegisteredSender(chainId, tryNativeToUint8Array(spoke.address, "ethereum")).then(wait);

    console.log(
      `Spoke ${chainId} registered on hub`
    )

    const tx2 = await spoke.setRegisteredSender(6, tryNativeToUint8Array(hub.address, "ethereum")).then(wait);
    console.log(`Hub registered on spoke ${chainId}`)
  }

  storeDeployedAddresses(deployed)
}

