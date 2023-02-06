import React, { useEffect, useState } from "react"
import useHackatonManagerFactory from "utils/context/hackatonManagerFactoryContext"

import Link from "next/link"
import Loader from "components/shared/Loader"
import Button from "components/shared/Button"

const listOfTech = ["Web", "No Code", "Low Code", "Blockchain", "AI", "MERN", "Solana"]

const ListHackaton = () => {
    const { listOfHackatons, initHackatonManagerFactory, loading } = useHackatonManagerFactory()

    useEffect(() => {
        initHackatonManagerFactory()
    }, [])

    function getRandomInt(min, max, toFixedNum = 0) {
        min = Math.ceil(min)
        max = Math.floor(max)
        let num = Math.floor(Math.random() * (max - min + 1)) + min
        if (toFixedNum) {
            const first2Str = String(num).slice(0, toFixedNum)
            const first2Num = Number(first2Str)
            return first2Num
        }
        return num
    }

    const HackatonComponent = ({ name, address }: any) => {
        return (
            <div className="flex flex-row justify-between w-fit bg-[#403F46] mb-4 px-4 py-2 rounded-xl">
                <div className="mr-12">
                    <h1 className="text-4xl">{name}</h1>
                    <div className="flex flex-row justify-start w-[25rem] px-4 mt-4 gap-6">
                        <div className="bg-[#007DFC] px-2 py-1 rounded-md">Online</div>
                        <div className="bg-[#007DFC] px-2 py-1 rounded-md">
                            {getRandomInt(100, 200)} Participants
                        </div>
                        <div className="bg-[#007DFC] px-2 py-1 rounded-md">
                            ${getRandomInt(10000, 20000, 2)}k in prizes
                        </div>
                    </div>
                    <div className="flex flex-row justify-start w-[25rem] px-4 mt-4 gap-6">
                        <div className="bg-[#3468ed] px-2 py-1 rounded-md">
                            {listOfTech[getRandomInt(0, listOfTech.length - 1)]}
                        </div>
                        <div className="bg-[#3468ed] px-2 py-1 rounded-md">
                            {listOfTech[getRandomInt(0, listOfTech.length - 1)]}
                        </div>
                        <div className="bg-[#3468ed] px-2 py-1 rounded-md">
                            {listOfTech[getRandomInt(0, listOfTech.length - 1)]}
                        </div>
                    </div>
                </div>
                <div className="flex flex-col justify-around items-center">
                    <Link href={`/manage-hackaton/${address}`}>
                        <Button className="w-44">Manage hackaton</Button>
                    </Link>
                    <Link href={`/hackaton/${address}`}>
                        <Button className="w-44">Join hackaton</Button>
                    </Link>
                </div>
            </div>
        )
    }

    return (
        <div className="container mx-auto">
            <h2 className="text-4xl my-5 font-bold text-gray-100">List of Hackatons</h2>
            {loading ? (
                <div className="flex justify-center">
                    <Loader />
                </div>
            ) : (
                <div className="grid grid-cols-5 gap-4">
                    <div className="col-span-2">
                        
                    {listOfHackatons ? (
                        listOfHackatons?.map((item) => (
                            <HackatonComponent name={item[0]} address={item[1]} />
                        ))
                    ) :
                    (
                        <span>Please connect to Altlayer-dev or Goerli</span>
                    )}
                            
                    
                    </div>
                </div>
            )}
        </div>
    )
}

export default ListHackaton
