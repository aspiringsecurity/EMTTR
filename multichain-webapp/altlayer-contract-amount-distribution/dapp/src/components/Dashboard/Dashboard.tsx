import React, { useEffect, useState } from "react"
import { DateTime } from "luxon"
import { useRouter } from "next/router"
import { ethers } from "ethers"
import Button from "components/shared/Button"
import Modal from "components/shared/Modal"
import JoinHackaton from "components/forms/JoinHackaton"
import SubmitProject from "components/forms/SubmitProject"
import useHackatonManager from "utils/context/hackatonManagerContext"
import Loader from "components/shared/Loader"
import { getDescription } from "utils/services/web3Storage"

const Dashboard = () => {
    const [deadline, setDeadline] = useState<string>("")
    const [isModalOpen, setIsModalOpen] = useState<boolean>(false)
    const [isSubmitModalOpen, setIsSubmitModalOpen] = useState<boolean>(false)
    const [description, setDescription] = useState<string>("")
    const { initHackatonManager, hackatonState, loading } = useHackatonManager()
    const { query } = useRouter()

    useEffect(() => {
        setDeadline(DateTime.now().toString())
    }, [])

    useEffect(() => {
        if (query.address) {
            initHackatonManager(query.address)
        }
    }, [query.address])

    const handleSetDescription = async () => {
        const desc = await getDescription(hackatonState.CID)
        setDescription(desc)
    }

    useEffect(() => {
        if (hackatonState.CID) {
            handleSetDescription()
        }
    }, [hackatonState.CID])

    const renderTracks = () =>
        hackatonState.tracks.map((track) => (
            <div>
                <h3 className="my-2 text-lg font-bold text-gray-100">{track.name}</h3>
                <p className="my-2 text-lg text-gray-100">
                    {ethers.utils.formatEther(track.poolAmount)} ETH
                </p>
            </div>
        ))

    return loading ? (
        <div className="flex justify-center py-10">
            <Loader />
        </div>
    ) : (
        <>
            <div className="p-4 bg-zinc-800 rounded-xl">
                <div className="container mx-auto">
                    <div className="grid grid-cols-5 gap-4">
                        <div className="col-span-3 h-full flex flex-col justify-between">
                            <h2 className="text-4xl font-bold text-gray-100">
                                {hackatonState.name}
                            </h2>
                            <p className="my-4 text-lg text-gray-400">{description}</p>
                            <div className="self-start">
                                <Button className="w-48" onClick={() => setIsModalOpen(true)}>
                                    Join Hackaton
                                </Button>
                            </div>
                        </div>
                        <div className="col-span-2 bg-zinc-700 p-3 rounded-2xl">
                            <div className="text-xs inline-flex items-center font-bold leading-sm uppercase px-3 py-1 bg-green-300 text-green-800 rounded-full">
                                17 days to deadline
                            </div>
                            <h3 className="my-4 text-lg font-bold text-gray-100">
                                Deadline {deadline}
                            </h3>
                            <Button className="mb-4" onClick={() => setIsSubmitModalOpen(true)}>
                                Submit project
                            </Button>
                            <div className="h-px w-full bg-zinc-400 "></div>
                            <div className="flex justify-between">
                                <h3 className="my-4 text-lg text-gray-200">
                                    <span className="font-bold">
                                        {ethers.utils.formatEther(hackatonState.funds)} ETH
                                    </span>{" "}
                                    in prizes
                                </h3>
                                <h3 className="my-4 text-lg text-gray-200">
                                    <span className="font-bold">25</span> participants
                                </h3>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <div className="container mx-auto mt-10">
                <div className="grid grid-cols-5 gap-4">
                    <div className="col-span-3">
                        <div className="flex items-center">
                            <h2 className="text-4xl font-bold text-gray-100">Prizes</h2>
                            <div className="ml-5 h-px w-full bg-zinc-400"></div>
                        </div>
                        <div className="grid my-4 grid-cols-2 gap-4">
                            <div className="col-span-2">
                                <h3 className="my-2 text-lg font-bold text-gray-100">
                                    {ethers.utils.formatEther(hackatonState.funds)} ETH in prizes
                                </h3>
                                <p className="my-2 text-lg text-gray-100">
                                    Prizes paid in cryptocurrency
                                </p>
                            </div>
                            {renderTracks()}
                        </div>
                    </div>
                </div>
            </div>

            <div className="container mx-auto mt-10">
                <div className="grid grid-cols-5 gap-4">
                    <div className="col-span-3">
                        <div className="flex items-center">
                            <h2 className="text-4xl font-bold text-gray-100">Participants</h2>
                            <div className="ml-5 h-px w-full bg-zinc-400"></div>
                        </div>
                        <div className="grid my-4 grid-cols-2 gap-4">
                            <div className="col-span-2">
                                <p className="my-2 text-lg text-gray-100">
                                    {hackatonState.participants.length ? (
                                        hackatonState.participants.map((item: any) => (
                                            <div key={item[1]}>
                                                <div className="mb-4 mt-4">
                                                    <h3 className="my-2 text-lg font-bold text-gray-100">
                                                        <div>Team name: {item[0]}</div>
                                                    </h3>
                                                    <div>Project name: {item[1]}</div>
                                                    <div>Project link: {item[2]}</div>
                                                    <div>Signup address: {item[6]}</div>{" "}
                                                </div>
                                                <hr />
                                            </div>
                                        ))
                                    ) : (
                                        <p className="my-2 text-lg text-gray-400">
                                            No participants yet
                                        </p>
                                    )}
                                </p>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
            <Modal title="Join Hackaton" isOpen={isModalOpen} onClose={() => setIsModalOpen(false)}>
                <div className="w-96">
                    <JoinHackaton />
                </div>
            </Modal>
            <Modal
                title="Submit project"
                isOpen={isSubmitModalOpen}
                onClose={() => setIsSubmitModalOpen(false)}
            >
                <div className="w-96">
                    <SubmitProject />
                </div>
            </Modal>
        </>
    )
}
export default Dashboard
