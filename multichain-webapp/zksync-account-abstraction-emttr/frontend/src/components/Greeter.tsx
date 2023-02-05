import React, { useEffect, useState } from "react";
import { render } from "react-dom";
import { BrowserRouter, Routes, Route } from "react-router-dom";

import Spinner from "./Spinner";
import Alert from "./Alert";

import { setGreeting, retrieveGreeting } from "../chain/interactions";

const Greeter = (props: { aaAddr: string; isOpen: boolean; close: any }) => {
    const [currentMessage, setCurrentMessage] = useState("fetching...");
    const [newMessage, setNewMessage] = useState("");
    const [amount, setAmount] = useState("0");

    const [alertInfo, setAlertInfo] = useState({
        isOpen: false,
        message: "",
        type: "successs",
    } as any);

    const [spinnerInfo, setSpinnerInfo] = useState({
        isOpen: false,
        message: "",
    } as any);
    const updateAlertInfo = (newValues: any) => {
        setAlertInfo({ ...alertInfo, ...newValues });
    };
    const updateSpinnerInfo = (newValues: any) => {
        setSpinnerInfo({ ...spinnerInfo, ...newValues });
    };
    const closeAlert = () => {
        updateAlertInfo({ isOpen: false });
    };
    const reload = async () => {
        const { err, greeting } = await retrieveGreeting();
        setCurrentMessage(greeting);
    };
    useEffect(() => {
        reload();
    }, []);

    if (props.isOpen == false) {
        return null;
    }

    return (
        <div className="GreeterContainer">
            <Alert
                isOpen={alertInfo.isOpen}
                message={alertInfo.message}
                type={alertInfo.type}
                close={closeAlert}
            ></Alert>
            <Spinner
                isOpen={spinnerInfo.isOpen && alertInfo.isOpen == false}
                message={spinnerInfo.message}
            ></Spinner>
            <div className="Greeter">
                <div className="Container0">
                    <h1>Greeter Interaction</h1>
                    <h1 className="CloseButton" onClick={() => props.close()}>
                        Close
                    </h1>
                </div>
                <div className="FieldsContainer">
                    <div className="Field">
                        <div className="Name">Current message:</div>
                        <div className="CurrentMessage">
                            <div className="Value">{currentMessage}</div>
                            <div className="Button1" onClick={reload}>
                                Refresh
                            </div>
                        </div>
                    </div>
                    <div className="Field">
                        <div className="Name">New message:</div>
                        <input
                            className="Input0"
                            onChange={(e) => setNewMessage(e.target.value)}
                        ></input>
                    </div>
                    <div className="Field">
                        <div className="Name">Amount:</div>
                        <input
                            className="Input0"
                            type="number"
                            onChange={(e) => setAmount(e.target.value)}
                        ></input>
                    </div>
                </div>
                <div
                    className="Button0"
                    onClick={async () => {
                        updateSpinnerInfo({
                            isOpen: true,
                            message: "Transaction signing pending...",
                        });

                        const { err } = await setGreeting({
                            aaAddr: props.aaAddr,
                            message: newMessage,
                            amount,
                        });
                        updateSpinnerInfo({ isOpen: false });
                        if (err == "") {
                            updateAlertInfo({
                                isOpen: true,
                                message: "Success: Transaction submitted",
                                type: "success",
                            });
                        } else {
                            updateAlertInfo({
                                isOpen: true,
                                message: `Error: ${err}`,
                                type: "error",
                            });
                        }
                    }}
                >
                    Submit
                </div>
            </div>
        </div>
    );
};

export default Greeter;
