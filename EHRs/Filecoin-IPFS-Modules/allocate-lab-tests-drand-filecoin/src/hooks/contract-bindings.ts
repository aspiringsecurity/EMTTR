import {ethers} from "ethers"
import {DRaffle} from "../../typechain-types"

export type Draw = {
    block: bigint,
    prize: bigint,
    winner: string
}
export const noWinner = "no winner"

// bindListener will listen for completed draws (both those with winners and those without), and call the associated callback
export function bindListener(contract: DRaffle, onDraw: (draw: Draw) => void) {
    contract.on(contract.filters.Winner(), (block, winner, amount) => {
        onDraw({
            block: block.toBigInt(),
            prize: amount.toBigInt(),
            winner: winner.toLowerCase(),
        })
    })
    contract.on(contract.filters.NoWinner(), (block) => {
        onDraw({
            block: block.toBigInt(),
            prize: BigInt(0),
            winner: noWinner,
        })
    })
}

// emits the next draw at the current time and listens for future draws
export function bindScheduledListener(contract: DRaffle, onScheduled: (block: bigint) => void) {
    contract.functions.nextDraw()
        .then(([next]) => onScheduled(next.toBigInt()))
        .catch(err => console.error(err))

    contract.on(contract.filters.Scheduled(), (block) => {
        onScheduled(block.toBigInt())
    })
}

export async function getAccounts(provider: ethers.providers.Web3Provider): Promise<Array<string>> {
    const accounts = await provider.send("eth_requestAccounts", [])
    if (Array.isArray(accounts)) {
        return accounts.map(each => each.toLowerCase())
    }
    return [accounts.toLowerCase()]
}