import * as React from "react"
import {ethers} from "ethers"
import {useEffect, useState} from "react"
import {bindScheduledListener} from "../hooks/contract-bindings"
import {useContract} from "../hooks/use-contract"
import {txGasLimit} from "../config"
import {Button} from "../components/Button"
import {LoadingSpinner} from "../components/LoadingSpinner"

type TriggerDrawProps = {
    provider: ethers.providers.Web3Provider
}

export const TriggerDraw = (props: TriggerDrawProps) => {
    const [isLoading, setLoading] = useState(false)
    const [currentBlock, setBlock] = useState(BigInt(0))
    const [nextDraw, setDraw] = useState(BigInt(0))
    const contract = useContract(props.provider)

    useEffect(() => {
        if (!contract) {
            return
        }
        bindScheduledListener(contract, (scheduledDraw) =>
            setDraw(scheduledDraw)
        )
        props.provider.on("block", (block) => setBlock(block))

    }, [props.provider, contract])

    async function triggerDraw() {
        setLoading(true)
        if (!contract) {
            console.error("you can't trigger a draw until the contract is bound to your metamask")
            return
        }
        try {
            const tx = await contract.draw({gasLimit: txGasLimit})
            await tx.wait(1)
        } finally {
            setLoading(false)
        }

    }

    if (isLoading) {
        return <LoadingSpinner/>
    }

    if (currentBlock < nextDraw - BigInt(1)) {
        return (
            <>
                <p>A button to trigger the next draw will appear here the block before the next draw.</p>
                <p>Click it before somebody else does to win a bounty!</p>
            </>
        )
    }
    return <Button onClick={triggerDraw} text={"Trigger draw"}/>
}