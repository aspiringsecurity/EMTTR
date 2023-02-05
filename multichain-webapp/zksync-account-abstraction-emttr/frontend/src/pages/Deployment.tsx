import React from "react";
import { useState } from "react";
import { BrowserRouter, Routes, Route, useNavigate } from "react-router-dom";

import { deployNewAA } from "../chain/interactions";
import Spinner from "../components/Spinner";

const Deployment = (props: { state: any; updateState: any }) => {
    const navigate = useNavigate();
    const [ownerAddr, setOwnerAddr] = useState("");
    const [displayMessage, setDisplayMessage] = useState("");

    return (
        <div className="Deployment">
            <h1>Deployment</h1>
            <div className="ProcedureInfo">
                <ol>
                    <li>
                        Enter the address of that will be the owner of this AA
                        <ul>
                            <li>
                                <b>Note</b>: doesn't have to be the same Address
                                with which you were logged in
                            </li>
                        </ul>
                    </li>
                    <li>
                        Press the <b>DEPLOY NEW AA</b> button
                    </li>
                    <li>
                        Sign the prompted transaction{" "}
                        <ul>
                            <li>
                                <b>Note</b>: Paymaster functionality not yet
                                implemented
                            </li>{" "}
                        </ul>
                    </li>
                </ol>
            </div>
            <div className="InputContainer">
                <input
                    className="Input0"
                    placeholder="Input owner's 0x-address"
                    onChange={(e) => {
                        setOwnerAddr(e.target.value);
                    }}
                ></input>

                <div
                    className="Button0"
                    onClick={async () => {
                        setDisplayMessage("Awaiting signature...");
                        const { err, aaAddr } = await deployNewAA({
                            ownerAddr,
                        });

                        setDisplayMessage("Broadcasting transaction...");
                        if (err == "") {
                            navigate(`/management/${aaAddr}`);
                            props.updateState({
                                monitoredAccounts:
                                    props.state.monitoredAccounts.push(aaAddr),
                            });
                        } else {
                            //TODO: display error message
                        }
                    }}
                >
                    Deploy new AA
                </div>
            </div>
            <Spinner
                isOpen={displayMessage != ""}
                message={displayMessage}
            ></Spinner>
        </div>
    );
};

export default Deployment;
