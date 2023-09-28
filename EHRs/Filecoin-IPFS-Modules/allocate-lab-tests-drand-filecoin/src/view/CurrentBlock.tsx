import * as React from "react"
import {useEffect, useState} from "react"
import {ethers} from "ethers"

type CurrentBlockProps = {
    provider: ethers.providers.Web3Provider
}
export const CurrentBlock = (props: CurrentBlockProps) => {
    const [block, setBlock] = useState<number>()

    useEffect(() => {
        props.provider.on("block", (blockNumber) => {
            console.log(`block number: ${blockNumber.toString()}`)
            if (!!blockNumber) {
                setBlock(blockNumber)
            }
        })
    }, [])

    return (
        <div>
            Current block: {block}
        </div>
    )
}