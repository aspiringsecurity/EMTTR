import * as React from "react"
import {useEffect, useState} from "react"
import {ethers} from "ethers"
import {useContract} from "../hooks/use-contract"
import {bindScheduledListener} from "../hooks/contract-bindings"

type ScheduledDrawsProps = {
    provider: ethers.providers.Web3Provider
}

export const ScheduledDraws = (props: ScheduledDrawsProps) => {
    const [nextDraw, setDraw] = useState<bigint>(BigInt(0))
    const contract = useContract(props.provider)

    useEffect(() => {
        if (!contract) {
            return
        }

        bindScheduledListener(contract, (scheduledBlock) =>
            setDraw(scheduledBlock)
        )
    }, [contract])

    if (nextDraw === BigInt(0)) {
        return <div>No draws scheduled</div>
    }
    return <div>Next draw at block: {nextDraw.toString()}</div>
}
