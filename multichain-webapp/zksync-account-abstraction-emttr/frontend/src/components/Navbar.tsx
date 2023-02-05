import React, { useEffect, useState } from "react";
import { render } from "react-dom";
import { BrowserRouter, Routes, Route, useNavigate } from "react-router-dom";

const Navbar = (props: { state: any; updateState: any }) => {
    const navigate = useNavigate();
    const { state, updateState } = props;

    return (
        <div className="Navbar">
            <div className="Container0" onClick={() => navigate("/")}>
                <h4 className="LogoText">[LayerHack]</h4>
                <img src="data:image/png;base64,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"></img>
                <h4 className="LogoText">zkSync - Account Abstraction</h4>
            </div>
            <div className="Container0">
                <h5 className="Signer">
                    {state.account != ""
                        ? `Selected Signer:   ${state.account.slice(
                              0,
                              10
                          )}...${state.account.slice(state.account.length - 5)}`
                        : null}
                </h5>
            </div>
        </div>
    );
};

export default Navbar;
