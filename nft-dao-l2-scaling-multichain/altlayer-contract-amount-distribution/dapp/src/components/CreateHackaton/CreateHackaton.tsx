import React, { useState } from "react"
import { ethers } from "ethers"
import useHackatonManagerFactory from "utils/context/hackatonManagerFactoryContext"
import CreateHackatonForm from "components/forms/CreateHackaton"
import Link from "next/link"

const CreateHackaton = () => {
    const { deploymentFee } = useHackatonManagerFactory()
    const [createdHackatonAddress, setCreatedHackatonAddress] = useState<string>("")

    return (
        <div className="container mx-auto py-10">
            <h2 className="text-4xl my-5 font-bold text-gray-100">Create Hackaton</h2>
            <p className="mb-4 text-lg text-gray-400">
                Hackaton deployment fee:{" "}
                {deploymentFee ? `${ethers.utils.formatEther(deploymentFee)} ETH` : null}
            </p>
            <div className="grid grid-cols-5 gap-4">
                <div className="col-span-2">
                    <CreateHackatonForm setCreatedHackatonAddress={setCreatedHackatonAddress} />
                </div>
                <div className="col-span-3">
                    {createdHackatonAddress ? (
                        <p className="mt-4 text-lg">
                            <span className="text-gray-100">
                                Created hackaton address:{" "}
                                <a
                                    rel="noopener noreferrer"
                                    target="_blank"
                                    className="underline"
                                    href={`https://goerli.etherscan.io/address/${createdHackatonAddress}`}
                                >
                                    {createdHackatonAddress}
                                </a>{" "}
                            </span>
                            <Link href={`/manage-hackaton/${createdHackatonAddress}`}>
                                <p className="cursor-pointer text-blue-300">
                                    add tracks and prizes
                                </p>
                            </Link>
                        </p>
                    ) : null}
                </div>
            </div>
        </div>
    )
}

export default CreateHackaton
