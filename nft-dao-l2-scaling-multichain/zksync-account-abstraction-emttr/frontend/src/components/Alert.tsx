import React from "react";
import { render } from "react-dom";
import { BrowserRouter, Routes, Route } from "react-router-dom";

const Alert = (props: {
    isOpen: boolean;
    message: string;
    type: "error" | "success";
    close: () => void;
}) => {
    if (props.isOpen === false) {
        return null;
    }
    setTimeout(props.close, 3000);
    return (
        <div className={`AlertContainer ${props.type}`}>
            <h3 className="Message">{props.message}</h3>
            <div className="Alert">
                <div></div>
            </div>
        </div>
    );
};

export default Alert;
