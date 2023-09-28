import {useEffect, useState} from "react"
import {ethers} from "ethers"
import {DRaffle, DRaffle__factory} from "../../typechain-types"
import {contractAddress} from "../config"

export function useContract(provider?: ethers.providers.Web3Provider): DRaffle | undefined {
    const [contract, setContract] = useState<DRaffle>()

    useEffect(() => {
        if (!provider) {
            return
        }

        provider.getCode(contractAddress)
            .catch(err => console.error("there was an error getting the code for the contract - are you sure you're using the correct contract ID?", err))

        console.log(`connecting to contract: ${contractAddress}`)
        setContract(DRaffle__factory.connect(contractAddress, provider.getSigner()))
    }, [provider])

    return contract
}