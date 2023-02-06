import React, { useEffect, useState } from "react";
import { render } from "react-dom";
import {
    BrowserRouter,
    Routes,
    Route,
    useNavigate,
    Link,
} from "react-router-dom";

import { init, getSigner, getAccount } from "../chain/interactions";

const Home = (props: { state: any; updateState: any }) => {
    const navigate = useNavigate();
    const { state, updateState } = props;
    const { loggedIn } = state;

    if (loggedIn) {
        return (
            <div className="Home">
                <h1>LayerHack Submission - Account Abstraction Plugins</h1>
                <h3>What is AAP</h3>
                Transaction executed through abstracted accounts are validated
                through a certain validation logic. This logic enables usages
                such as requiring multiple signatures for a transaction.
                However, this logic is stored on the deployed account smart
                contract and as such cannot be changed after the fact.
                <br></br>
                <br></br>
                AAP enables customization of verification logic via plugins.
                This allows users to set up rules which their account has to
                abide by when executing transactions while being able to
                reconfigure them in the future. The process improves account
                security by restricting permissions certain keys have while
                remaining adaptable to the user's changing needs.
                <br></br>
                <br></br>Usage examples:
                <ul>
                    <li>
                        Enforcing a spending limit based on the key used for the
                        transaction (I can only spend XX on my mobile device,
                        but I can spend XXX on my desktop device).{" "}
                    </li>
                    <li>
                        Restricting interaction with a certain dApp to a
                        specific key (only my PC can alter my DeSo profile){" "}
                    </li>
                    <li>
                        Defining session keys, which are allowed to initiate
                        transactions for only a limited period of time.
                    </li>
                </ul>
                <br></br>
                Full explanation and code available at:{" "}
                <a
                    href="https://github.com/0x3327/layerhack_2022"
                    target="_blank"
                >
                    Github repo
                </a>
                <br></br> <br></br> <br></br>
                <div className="Entry">
                    <div
                        className="Button0"
                        onClick={() => navigate("/deployment")}
                    >
                        Deploy new AA
                    </div>
                </div>
                <div className="Entry">
                    <div
                        className="Button0"
                        onClick={() => navigate("/management")}
                    >
                        Manage an existing AA
                    </div>
                </div>
            </div>
        );
    } else {
        return (
            <div className="Home">
                <h1>LayerHack Submission - Account Abstraction Plugins</h1>
                <h3>What is AAP</h3>
                Transaction executed through abstracted accounts are validated
                through a certain validation logic. This logic enables usages
                such as requiring multiple signatures for a transaction.
                However, this logic is stored on the deployed account smart
                contract and as such cannot be changed after the fact.
                <br></br>
                <br></br>
                AAP enables customization of verification logic via plugins.
                This allows users to set up rules which their account has to
                abide by when executing transactions while being able to
                reconfigure them in the future. The process improves account
                security by restricting permissions certain keys have while
                remaining adaptable to the user's changing needs.
                <br></br>
                <br></br>Usage examples:
                <ul>
                    <li>
                        Enforcing a spending limit based on the key used for the
                        transaction (I can only spend XX on my mobile device,
                        but I can spend XXX on my desktop device).{" "}
                    </li>
                    <li>
                        Restricting interaction with a certain dApp to a
                        specific key (only my PC can alter my DeSo profile){" "}
                    </li>
                    <li>
                        Defining session keys, which are allowed to initiate
                        transactions for only a limited period of time.
                    </li>
                </ul>
                <br></br>
                Full explanation and code available at:{" "}
                <a
                    href="https://github.com/0x3327/layerhack_2022"
                    target="_blank"
                >
                    Github repo
                </a>
                <br></br> <br></br> <br></br>
                <div className="Entry">
                    <div
                        className="Button0"
                        onClick={async () => {
                            const { err } = await init({ updateState });
                            if (err == "") {
                                const { err, account } = getAccount();
                                if (err == "") {
                                    updateState({ loggedIn: true, account });
                                }
                            }
                        }}
                    >
                        Connect Metamask
                    </div>
                    <h3>Note: You need to be on ZkSync's alpha testnet</h3>
                </div>
            </div>
        );
    }
};

export default Home;
