import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter } from "react-router-dom";
import "./index.css";

// We import bootstrap here, but you can remove if you want
import "bootstrap/dist/css/bootstrap.css";
import { Navigation } from "./components/Navigation";
import { DappWrapper } from "./components/DappWrapper";

// This is the entry point of your application, but it just renders the Dapp
// react component. All of the logic is contained in it.

ReactDOM.render(
  <React.StrictMode>
    <BrowserRouter>
      <Navigation />
      <div className="container p-4">
        <DappWrapper />
      </div>
    </BrowserRouter>
  </React.StrictMode>,
  document.getElementById("root")
);