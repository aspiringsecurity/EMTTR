import React, { useEffect, useState } from "react"
import { useRouter } from "next/router"
import { ethers } from "ethers"
import HackatonMainData from "components/forms/HackatonMainData"
import HackatonExtraData from "components/forms/HackatonExtraData"
import FundHackaton from "components/forms/FundHackaton"
import useHackatonManager from "utils/context/hackatonManagerContext"
import Loader from "components/shared/Loader"
import { getDescription } from "utils/services/web3Storage"
import useWallet from "utils/context/walletContext"
import SetWinner from "components/forms/SetWinner"
import AddPrizeToTrack from "components/forms/AddPrizeToTrack"

const HackatonDetails = () => {
    const [description, setDescription] = useState<string>("")
    const {
        initHackatonManager,
        hackatonState,
        loading,
        retrieveDescription,
        isCommitteMember,
        contract,
    } = useHackatonManager()
    const { query } = useRouter()
    const { wallet } = useWallet()
    const [editable, setEditable] = useState<boolean>(false)

    useEffect(() => {
        if (query.address) {
            initHackatonManager(query.address)
        }
    }, [query.address])

    useEffect(() => {
        if (wallet.address && contract) {
            ;(async () => {
                const isMember = await isCommitteMember(contract, wallet.address)
                setEditable(isMember)
            })()
        }
    }, [wallet.address, contract])

    useEffect(() => {
        if (hackatonState.CID) {
            handleSetDescription()
        }
    }, [hackatonState.CID])

    const handleSetDescription = async () => {
        const desc = await getDescription(hackatonState.CID)
        setDescription(desc)
    }

    const renderTracks = () =>
        hackatonState.tracks.length ? (
            <div>
                <div className="flex items-center mt-4">
                    <h2 className="text-xl font-bold text-gray-200 whitespace-nowrap">
                        Existing tracks
                    </h2>
                    <div className="ml-5 h-px w-full bg-zinc-400"></div>
                </div>
                <div className="grid grid-cols-4 gap-4">
                    {hackatonState.tracks.map((track) => (
                        <div key={track.name} className="col-span-2 mt-2">
                            <h3 className="mt-1 font-bold text-gray-200">{track.name}</h3>
                            <p className="mt-1 text-gray-400">
                                {ethers.utils.formatEther(track.poolAmount)} ETH
                            </p>
                        </div>
                    ))}
                </div>
                <div className="mt-4 mb-10 h-px w-full bg-zinc-400"></div>
            </div>
        ) : null

    const renderForms = () => {
        if (hackatonState.funded) {
            return (
                <div className="grid grid-cols-5 gap-4">
                    <div className="col-span-3 bg-zinc-800 px-10 py-10">
                        <h2 className="text-4xl my-5 font-bold text-gray-100">Manage Hackaton</h2>
                        <p className="mb-2 text-lg text-gray-400">
                            Total amount of funds: {ethers.utils.formatEther(hackatonState.funds)}{" "}
                            ETH
                        </p>
                        <p className="mb-6 text-lg text-gray-400">
                            Distributed funds:{" "}
                            {ethers.utils.formatEther(hackatonState.fundedByTracks)} ETH,{" "}
                            {ethers.utils.formatEther(
                                hackatonState.funds?.sub(hackatonState.fundedByTracks)
                            )}{" "}
                            ETH remains
                        </p>
                        {renderTracks()}
                        <div className="flex items-center my-4">
                            <h2 className="text-xl font-bold text-gray-200 whitespace-nowrap">
                                Add tracks
                            </h2>
                            <div className="ml-5 h-px w-full bg-zinc-400"></div>
                        </div>{" "}
                        <HackatonMainData />

                        <div className="flex items-center mt-4">
                            <h2 className="text-xl font-bold text-gray-200 whitespace-nowrap">
                                Add prizes
                            </h2>
                            <div className="ml-5 h-px w-full bg-zinc-400"></div>
                        </div>
                        <AddPrizeToTrack />

                        <div className="flex items-center mt-4">
                            <h2 className="text-xl font-bold text-gray-200 whitespace-nowrap">
                                Pick winner
                            </h2>
                            <div className="ml-5 h-px w-full bg-zinc-400"></div>
                        </div>
                        <SetWinner />
                    </div>
                    <div className="col-span-2 py-10">
                        <h2 className="text-4xl my-5 font-bold text-gray-100">
                            Add extra data for hackaton
                        </h2>
                        <HackatonExtraData />
                    </div>
                </div>
            )
        } else {
            return (
                <div className="grid grid-cols-5 gap-4">
                    <div className="col-span-3 bg-zinc-800 px-10 py-10">
                        <h2 className="text-4xl my-5 font-bold text-gray-100">Fund Hackaton</h2>
                        <FundHackaton />
                    </div>
                </div>
            )
        }
    }

    return (
        <div className="container mx-auto py-10">
            {!wallet.address ? (
                <h2 className="text-4xl my-5 font-bold text-gray-100 text-center">
                    Connect your wallet first!
                </h2>
            ) : (
                <>
                    {loading ? (
                        <div className="flex justify-center">
                            <Loader />
                        </div>
                    ) : (
                        <>
                            {editable ? (
                                <>
                                    <h2 className="text-4xl my-5 font-bold text-gray-100">
                                        {hackatonState.name}
                                    </h2>
                                    <p className="text-xl my-5 text-gray-400">{description}</p>
                                    {renderForms()}
                                </>
                            ) : (
                                <h2 className="text-4xl my-5 font-bold text-gray-100 text-center">
                                    Access denied, only committee members can edit hackaton
                                </h2>
                            )}
                        </>
                    )}
                </>
            )}
        </div>
    )
}

export default HackatonDetails
