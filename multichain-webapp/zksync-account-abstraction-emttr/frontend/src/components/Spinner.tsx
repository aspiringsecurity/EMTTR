import React from "react";
import { render } from "react-dom";
import { BrowserRouter, Routes, Route } from "react-router-dom";

const Spinner = (props: { isOpen: boolean; message: string }) => {
    if (props.isOpen === false) {
        return null;
    }
    return (
        <div className="SpinnerContainer">
            <h3 className="Message">{props.message}</h3>
            <div className="Spinner">
                <div></div>
            </div>
        </div>
    );
};

export default Spinner;
