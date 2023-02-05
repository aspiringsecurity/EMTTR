import React from "react";
import { render } from "react-dom";
import { BrowserRouter, Routes, Route } from "react-router-dom";

import { deactivateLimitPlugin } from "../../chain/interactions";

const LimitPluginInfo = (props: {
    aaAddr: string;
    authority: string;
    limit: string;
}) => {
    return (
        <div className="Plugin">
            <div className="FieldsContainer">
                <div className="Field">
                    <div className="Identifier">Name:</div>
                    <div className="Value">Enforced limit</div>
                </div>
                <div className="Field">
                    <div className="Identifier">Spender:</div>
                    <div className="Value">{props.authority}</div>
                </div>
                <div className="Field">
                    <div className="Identifier">Limit:</div>
                    <div className="Value">{props.limit}</div>
                </div>
            </div>
            <div className="Container0">
                <div></div>
                <div
                    className="Button1"
                    onClick={async () => {
                        await deactivateLimitPlugin(props);
                    }}
                >
                    Deactivate
                </div>
            </div>
        </div>
    );
};

export default LimitPluginInfo;
