import { ethers, BigNumber, Contract, Signer } from "ethers"
import { createContext, useContext, useMemo, useState } from "react"

import abi from "constants/abi/hackatonManagerAbi"
// import abi from "../../../../contracts/artifacts/contracts/HackathonManager.sol/HackathonManager.json"

// import { retrieve } from "utils/services/web3Storage"

export type Track = {
    trackName: string
    trackPrize: string
}

export type PriceForTrack = {
    trackName: string
    prizeName: string
    prizeAmount: string
}

type OnChainTrack = {
    name: string
    poolAmount: number
    prizeTotal: number
}

export type Participant = {
    teamName: string
    projectName: string
    projectLink: string
}

type HackatonState = {
    name: string
    description: string
    fundedByTracks: null | BigNumber
    CID: string
    funds: null | BigNumber
    funded: boolean
    participants: Participant[]
    tracks: OnChainTrack[]
}

type HackatonManager = {
    loading: boolean
    hackatonState: HackatonState
    initHackatonManager: (address: string) => void
    createSignedContract: (signer: Signer) => void
    resetSignedContract: () => void
    createTracks: (tracks: Track) => void
    addPrizeToTrack: (priceForTrack: PriceForTrack) => void
    registerParticipant: (participant: Participant) => void
    addCID: (CID: string) => void
    fundHackaton: (amount: string) => void
    isCommitteMember: (contract: Contract, address: string) => void
    setWinner: (trackName: string, prize: string, teamName: string) => void
    submitProject: (teamName: string) => void
    contract: Contract
}

const defaultHackatonState: HackatonState = {
    name: "",
    fundedByTracks: null,
    description: "",
    CID: "",
    funds: null,
    funded: false,
    tracks: [],
    participants: [],
}

export const HackatonManagerContext = createContext<HackatonManager>({})

export const useHackatonManagerContext = () => {
    const [contract, setContract] = useState<Contract>()
    const [signedContract, setSignedContract] = useState<Contract>()
    const [loading, setLoading] = useState<boolean>(true)
    const [contractAddress, setContractAddress] = useState<string>("")
    const [hackatonState, setHackatonState] = useState<HackatonState>(defaultHackatonState)

    const createSignedContract = (signer: Signer) => {
        const contract_ = new ethers.Contract(contractAddress, abi.abi, signer)
        setSignedContract(contract_)
    }

    const resetSignedContract = () => {
        setSignedContract(undefined)
    }

    const updateHackatonState = async (contract_) => {
        const [name, CID, funds] = await Promise.all([
            contract_._hackathonName(),
            contract_.getCID(),
            await contract_._hackathonFundBalance(),
        ])
        const funded = funds.gt(0)
        const length = (await contract_.getCurrentMaxIndexOfTracks()).toNumber()
        const tracks: OnChainTrack[] = []
        let fundedByTracks = BigNumber.from(0)

        for (let i = 0; i < length; i++) {
            const trackName = await contract_.getTrackByIndex(i)
            const track = await contract_._hackathonTracks(trackName)
            tracks.push({
                name: track._trackName,
                poolAmount: track._trackPoolAmount,
                prizeTotal: track._currentPrizeTotal,
            })
            fundedByTracks = fundedByTracks.add(track._trackPoolAmount)
        }

        const participants = []
        const participantsLength = await contract_.ParticipantsLength()

        for (let i = 0; i < participantsLength; i++) {
            const participant = await contract_._participants(i)
            participants.push(participant)
        }

        let description = ""
        if (CID) {
            // const res = await retrieve(CID)
            // const files = await res?.files()
            // if (files && files.length) {
            //     const json = JSON.parse(await files[0].text())
            //     description = json.description
            // }
        }
        setHackatonState({
            fundedByTracks,
            name,
            CID,
            funds,
            funded,
            description,
            tracks,
            participants,
        })
    }

    const isCommitteMember = async (contract: Contract, address: string) => {
        const isMember = await contract._hackathonCommitteeMembers(address)
        return isMember
    }

    const initHackatonManager = async (contractAddress: string) => {
        setLoading(true)
        const provider = new ethers.providers.Web3Provider(window.ethereum)
        const contract_ = new ethers.Contract(contractAddress, abi.abi, provider)
        setContract(contract_)
        await updateHackatonState(contract_)
        setContractAddress(contractAddress)
        setLoading(false)
    }

    const createTracks = async (track: Track) => {
        if (signedContract) {
            const rc = await signedContract.createTrack(
                track.trackName,
                ethers.utils.parseEther(track.trackPrize)
            )
            await rc.wait()
            await updateHackatonState(contract)
        }
    }

    const addPrizeToTrack = async (priceForTrack: PriceForTrack) => {
        if (signedContract) {
            const rc = await signedContract.addPrizeToTrack(
                priceForTrack.trackName,
                priceForTrack.prizeName,
                ethers.utils.parseEther(priceForTrack.prizeAmount)
            )
            await rc.wait()
            await updateHackatonState(contract)
        }
    }

    const registerParticipant = async (participant: Participant) => {
        if (signedContract) {
            const rc = await signedContract.registerParticipant(
                participant.teamName,
                participant.projectName,
                participant.projectLink
            )
            await rc.wait()
            await updateHackatonState(contract)
        }
    }

    const fundHackaton = async (amount: string) => {
        if (signedContract) {
            const rc = await signedContract.fundHackathon({
                value: ethers.utils.parseEther(amount),
            })
            await rc.wait()
            setHackatonState({
                ...hackatonState,
                funded: true,
            })
            await updateHackatonState(contract)
        }
    }

    const setWinner = async (trackName: string, prize: string, teamName: string) => {
        if (signedContract) {
            const rc = await signedContract.captureWinner(trackName, prize, teamName)
            await rc.wait()
            await updateHackatonState(contract)
        }
    }

    const submitProject = async (teamName: string) => {
        if (signedContract) {
            const rc = await signedContract.submitProject(teamName)
            await rc.wait()
            await updateHackatonState(contract)
        }
    }

    const addCID = async (CID: string) => {
        if (signedContract) {
            const rc = await signedContract.addCID(CID)
            await rc.wait()
            await updateHackatonState(contract)
        }
    }

    const hackatonManagerContext = useMemo(
        () => ({
            loading,
            initHackatonManager,
            createTracks,
            addPrizeToTrack,
            createSignedContract,
            resetSignedContract,
            registerParticipant,
            addCID,
            fundHackaton,
            hackatonState,
            isCommitteMember,
            setWinner,
            submitProject,
            contract,
        }),
        [
            loading,
            hackatonState,
            initHackatonManager,
            createTracks,
            addPrizeToTrack,
            createSignedContract,
            resetSignedContract,
            registerParticipant,
            addCID,
            fundHackaton,
            isCommitteMember,
            setWinner,
            submitProject,
            contract,
        ]
    )

    return hackatonManagerContext
}

const useHackatonManager = () => {
    return useContext(HackatonManagerContext)
}

export default useHackatonManager
