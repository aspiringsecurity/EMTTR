import * as React from "react"
import {ethers} from "ethers"
import {entryCost, triggerIncentive} from "../config"

const entryCostFIL = ethers.utils.formatEther(entryCost)
const incentiveFIL = ethers.utils.formatEther(triggerIncentive)
export const Info = () =>
    <div>
        <p>
            Welcome to the <a href={"https://drand.love"}>drand</a> raffle!<br/>
            This uses a solidity smart contract that can be deployed both FVM and EVM.<br/>
            Find the code on <a href={"https://github.com/drand/draffle"}>github</a>.
        </p>
        <h3>How does it work?</h3>
        <p>
            Draws are made periodically at a given block height.<br/>
            Entries can be made for draw up to 3 blocks before the draw is scheduled to happen<br/>
            You can enter by clicking the button below - it costs {entryCostFIL}FIL. The {entryCostFIL}FIL goes into a
            pot that gets paid out
            to the winner<br/>
            Additionally, whoever submits the transaction that triggers the draw gets a bounty of {incentiveFIL}FIL from the
            pot!
        </p>
    </div>
