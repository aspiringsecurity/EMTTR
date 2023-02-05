import React, { useEffect, useState } from "react";
import { render } from "react-dom";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import {
    createLimitPlugin,
    addLimitPlugin,
    hasLimitPlugin,
} from "../../chain/interactions";

import Spinner from "../Spinner";

const LimitPluginCreation = (props: {
    aaAddr: string;
    isOpen: boolean;
    close: any;
}) => {
    const [spinnerOpen, setSpinnerOpen] = useState(false);
    const [spinnerMessage, setSpinnerMessage] = useState("");
    const [hasThePluginActivated, setHasThePluginActivated] = useState(false);

    const [authority, setAuthority] = useState("");
    const [limit, setLimit] = useState("");

    useEffect(() => {
        (async () => {
            const { err, hasIt } = await hasLimitPlugin(props);
            setHasThePluginActivated(hasIt);
        })();
    });

    if (props.isOpen == false) {
        return null;
    }
    return (
        <div className="LimitPluginCreationContainer">
            <Spinner isOpen={spinnerOpen} message={spinnerMessage}></Spinner>
            <div className="LimitPluginCreation">
                <div className="Container0">
                    <h1>Ether Limit Plugin</h1>
                    {hasThePluginActivated == false ? (
                        <h1
                            className="CloseButton"
                            onClick={async () => {
                                await addLimitPlugin({ aaAddr: props.aaAddr });
                                const { err, hasIt } = await hasLimitPlugin(
                                    props
                                );
                                setHasThePluginActivated(hasIt);
                            }}
                        >
                            Activate Plugin
                        </h1>
                    ) : (
                        <h1 className="GreenButton">Activated Plugin</h1>
                    )}
                    <h1
                        className="CloseButton"
                        onClick={() => {
                            props.close();
                        }}
                    >
                        Close
                    </h1>
                </div>
                <div className="FieldsContainer">
                    <div className="Field">
                        <div className="Name">Spender:</div>
                        <input
                            className="Input0"
                            onChange={(e) => setAuthority(e.target.value)}
                        ></input>
                    </div>
                    <div className="Field">
                        <div className="Name">Amount:</div>
                        <input
                            className="Input0"
                            onChange={(e) => setLimit(e.target.value)}
                        ></input>
                    </div>
                </div>
                <div
                    className="Button0"
                    onClick={async () => {
                        setSpinnerOpen(true);
                        setSpinnerMessage("Creating Limit Plugin");
                        const { err } = await createLimitPlugin({
                            aaAddr: props.aaAddr,
                            address: authority,
                            limit,
                        });
                        if (err == "") {
                            //alert:TODO
                            setSpinnerOpen(false);
                            props.close();
                        }
                    }}
                >
                    Set Limit
                </div>
            </div>
        </div>
    );
};

export default LimitPluginCreation;
