import { useEffect, useState } from "react";
import { render } from "react-dom";
import { BrowserRouter, Routes, Route, Navigate } from "react-router-dom";
import { MetaMaskInpageProvider } from "@metamask/providers";

import Home from "./pages/Home";
import Deployment from "./pages/Deployment";
import Management from "./pages/Management";

import Navbar from "./components/Navbar";

import "./style/css/App.css";
import "./style/css/config.css";

import { init } from "./chain/interactions";

declare global {
    interface Window {
        ethereum?: MetaMaskInpageProvider;
    }
}
const App = () => {
    const [state, setState] = useState({
        loggedIn: false,
        account: "",
        monitoredAccounts: [],
    } as any);

    const updateState = (newValues: any) => {
        const newState = { ...state, ...newValues };
        localStorage.setItem("aa-state", JSON.stringify(newState));
        console.log(newState);
        setState(newState);
    };

    useEffect(() => {
        (async () => {
            const prevState = localStorage.getItem("aa-state");
            if (prevState != null) {
                setState(JSON.parse(prevState as string));
                await init({ updateState });
            }
            const { ethereum } = window;
            if (ethereum != null)
                ethereum.on("accountsChanged", async (accounts) => {
                    // props.updateState({ account: (accounts as any)[0] });
                    await init({ updateState });
                });
        })();
    }, []);

    return (
        <BrowserRouter>
            <div className="App">
                <Navbar state={state} updateState={updateState}></Navbar>
                <Routes>
                    <Route
                        path="/"
                        element={
                            <Home state={state} updateState={updateState} />
                        }
                    ></Route>

                    <Route
                        path="/deployment"
                        element={
                            <Deployment
                                state={state}
                                updateState={updateState}
                            />
                        }
                    ></Route>
                    <Route
                        path="/management"
                        element={
                            <Management
                                state={state}
                                updateState={updateState}
                            />
                        }
                    ></Route>
                    <Route
                        path="/management/:initialAaAddr"
                        element={
                            <Management
                                state={state}
                                updateState={updateState}
                            />
                        }
                    ></Route>

                    <Route path="*" element={<Navigate to="/" />}></Route>
                </Routes>
            </div>
        </BrowserRouter>
    );
};

export default App;
