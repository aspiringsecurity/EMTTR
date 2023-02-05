/* eslint-disable react-hooks/exhaustive-deps */
import { ethers, BigNumber, Contract, Signer } from "ethers"
import { createContext, useContext, useMemo, useState } from "react"

import abi from "constants/abi/hackatonManagerFactoryAbi"
// import abi from "../../../../contracts/artifacts/contracts/HackathonManagerFactory.sol/HackathonManagerFactory.json"
import contractAddresses from "constants/contractAddresses.json"

type HackatonManagerFactory = {
    deploymentFee: undefined | BigNumber
    listOfHackatons: undefined | Array<[string, string]>
    loading: boolean
    initHackatonManagerFactory: () => void
    createNewHack: (name: string) => string
    createSignedContract: (signer: Signer) => void
    resetSignedContract: () => void
    getHackContractAddress: (name: string) => Promise<string | undefined>
}

export const HackatonManagerFactoryContext = createContext<HackatonManagerFactory>({})

export const useHackatonManagerFactoryContext = () => {
    const [signedContract, setSignedContract] = useState<Contract>()
    const [loading, setLoading] = useState<boolean>(false)
    const [deploymentFee, setDeploymentFee] = useState<BigNumber>()
    const [listOfHackatons, setListOfHackatons] = useState<Array<[string, string]>>()

    const createSignedContract = async (signer: Signer) => {
        const contractAddr = contractAddresses.hackatonManagerFactoryContract[await signer.getChainId()]
        
        if(contractAddr === undefined)
        {
            console.log("wrong chain")
            return
        }

        const сontract_ = new ethers.Contract(
            contractAddresses.hackatonManagerFactoryContract[await signer.getChainId()],
            abi.abi,
            signer
        )
        
        setSignedContract(сontract_)
    }

    const resetSignedContract = () => {
        setSignedContract(undefined)
    }

    const initHackatonManagerFactory = async () => {
        setLoading(true)
        const provider = new ethers.providers.Web3Provider(window.ethereum)
        const chainId = (await provider.getNetwork()).chainId
        const contractAddr = contractAddresses.hackatonManagerFactoryContract[chainId]
        
        if(contractAddr === undefined)
        {
            console.log("wrong chain")
            setLoading(false)    
            return;
        }

        console.log(contractAddr)
        const contract_ = new ethers.Contract(
            contractAddr,
            abi.abi,
            provider
        )

        const [deploymentFee] = await Promise.all([contract_.deploymentFee()])
        const listOfHackatons = await getListOfHackatons(contract_)

        setListOfHackatons(listOfHackatons)
        setDeploymentFee(deploymentFee)
        setLoading(false)
    }

    const getListOfHackatons = async (contract: Contract) => {
        const length = await contract.hackathonLength()
        const hackatons: Array<[string, string]> = []

        for (let i = 0; i < length; i++) {
            const name: string = await contract.hackathonNames(i)
            const address: string = await contract.getHackContractAddress(name)
            hackatons.push([name, address])
        }

        return hackatons
    }

    const createNewHack = async (name: string) => {
        if (signedContract) {
            setLoading(true)
            const txReceipt = await signedContract.createNewHack(name, {
                value: deploymentFee,
            })
            const res = await txReceipt.wait()
            const event = res.events?.find(
                (event: { event: string }) => event.event === "HackCreated"
            )
            const hackatonAddress = event?.args?._contractAddress
            setLoading(false)
            return hackatonAddress
        }
    }

    const getHackContractAddress = async (name: string) => {
        if (signedContract) {
            const address = await signedContract.getHackContractAddress(name)
            return address
        }
    }

    const hackatonManagerFactoryContext = useMemo(
        () => ({
            deploymentFee,
            listOfHackatons,
            loading,
            initHackatonManagerFactory,
            createNewHack,
            getHackContractAddress,
            createSignedContract,
            resetSignedContract,
        }),
        [
            deploymentFee,
            listOfHackatons,
            loading,
            initHackatonManagerFactory,
            createNewHack,
            getHackContractAddress,
            createSignedContract,
            resetSignedContract,
        ]
    )

    return hackatonManagerFactoryContext
}

const useHackatonManagerFactory = () => {
    return useContext(HackatonManagerFactoryContext)
}

export default useHackatonManagerFactory
