import * as React from "react"
import {useWeb3} from "./hooks/use-web3"
import {CurrentBlock} from "./view/CurrentBlock"
import {PastDraws} from "./view/PastDraws"
import {ScheduledDraws} from "./view/ScheduledDraws"
import {EnterDraw} from "./view/EnterDraw"
import {Info} from "./view/Info"
import {Split} from "./view/Split"
import {CSSProperties} from "react"
import {TriggerDraw} from "./view/TriggerDraw"
import {Logo} from "./view/Logo"
import {LoadingSpinner} from "./components/LoadingSpinner"

const style: CSSProperties = {
    width: "100%",
    display: "flex",
    flexDirection: "column",
    alignItems: "center"
}
export const App = () => {
    const {provider, error, isLoading} = useWeb3()

    if (isLoading) {
        return <LoadingSpinner />
    }
    if (!!error) {
        return <div>Error loading metamask: {error}</div>
    }
    if (!provider) {
        return <div>There was an error loading your metamask... is it installed?</div>
    }
    return (
        <div className={"container"} style={style}>
            <div className={"row text-left"}>
                <Logo/>
                <Info/>
            </div>

            <div className={"row w-100 m-0"}>
                <div className={"col-lg-6 col-sm-12 p-0"}>
                    <div className={"row-lg-6 row-sm-12 pt-4"}>
                        <h3>Next draw</h3>
                        <CurrentBlock provider={provider}/>
                        <ScheduledDraws provider={provider}/>
                        <EnterDraw provider={provider}/>
                    </div>
                    <div className={"row-lg-6 row-sm-12 pt-4"}>
                        <PastDraws provider={provider}/>
                    </div>
                </div>

                <div className={"col-lg-6 col-sm-12 pt-4"}>
                    <h3>Trigger draw</h3>
                    <TriggerDraw provider={provider}/>
                </div>
            </div>
        </div>
    )
}