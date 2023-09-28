import * as React from "react"
import {useEffect, useState} from "react"
import {ethers} from "ethers"
import {useContract} from "../hooks/use-contract"
import {DRaffle} from "../../typechain-types"
import {bindListener, Draw, getAccounts, noWinner} from "../hooks/contract-bindings"

type PastDrawsProps = {
    provider: ethers.providers.Web3Provider
}

export const PastDraws = (props: PastDrawsProps) => {
    const [draws, setDraws] = useState<Array<Draw>>([])
    const [accounts, setAccounts] = useState<Array<string>>([])
    const contract = useContract(props.provider)

    const addDraw = (draw: Draw) => {
        // we put the newest draw first, so it appears top of the list
        setDraws([draw, ...draws])
    }

    useEffect(() => {
        if (!props.provider || !contract) {
            return
        }

        // we get the accounts for the associated metamask wallet to see if we won any of the draws
        getAccounts(props.provider).then(accs => setAccounts(accs))

        getPastDraws(props.provider, contract).then(sortedDraws => setDraws(sortedDraws))

        bindListener(contract, (draw) => addDraw(draw))

    }, [props.provider, contract])

    if (draws.length === 0) {
        return <div><h3>Past draws</h3><p>there have been no draws yet!</p></div>
    }
    return (
        <div>
            <h3>Past draws</h3>
            {draws.map(d => <Entry draw={d} accounts={accounts}/>)}
        </div>
    )
}

type EntryProps = {
    draw: Draw,
    accounts: Array<string>
}

function Entry(props: EntryProps) {
    if (props.draw.winner === noWinner) {
        return (
            <div>
                <div>block: {props.draw.block.toString()}</div>
                <div>No entries!</div>
            </div>
        )
    }

    const prize = ethers.utils.formatEther(props.draw.prize)

    if (props.accounts.includes(props.draw.winner)) {
        return (
            <div>
                <div>block: {props.draw.block.toString()}</div>
                <div>winner: You! 🎉</div>
                <div>prize: {prize}FIL</div>
            </div>
        )
    }

    return (
        <div>
            <div>block: {props.draw.block.toString()}</div>
            <div>winner: {props.draw.winner}</div>
            <div>prize: {prize}FIL</div>
        </div>
    )
}

async function getPastDraws(provider: ethers.providers.Web3Provider, contract: DRaffle): Promise<Array<Draw>> {
    // to prevent DoS for big queries, most providers only allow a 24 hour lookback on events,
    // so let's calculate how many blocks that and take off 1 to be safe
    const blocksInADay =  24 * 60 * 2 - 1
    const startingBlock = provider.blockNumber - blocksInADay
    const output: Array<Draw> = []
    const noWinEvents = await contract.queryFilter(contract.filters.NoWinner(), startingBlock, provider.blockNumber)
    noWinEvents.forEach(e => {
        output.push({
                block: e.args[0].toBigInt(),
                prize: BigInt(0),
                winner: noWinner,
            }
        )
    })

    const winEvents = await contract.queryFilter(contract.filters.Winner(), startingBlock, provider.blockNumber)
    winEvents.forEach(e => output.push({
            block: e.args[0].toBigInt(),
            winner: e.args[1].toLowerCase(),
            prize: e.args[2].toBigInt(),
        }
    ))

    // we want them in reverse order so the most recent is at the top
    return output.sort((a, b) => Number(b.block - a.block))
}
