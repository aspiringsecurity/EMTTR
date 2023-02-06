import React from "react";
import * as ReactDOMClient from "react-dom/client";
import "./assets/css/tribunal.css";
import Router from "./Router";
import { BrowserRouter } from "react-router-dom";
import { store } from "./redux/store";
import { Provider } from "react-redux";
import MoralisInitProvider from "./context/MoralisInitContext";
import UserContextProvider from "./context/UserContext";

const container = document.getElementById("root");

// Create a root.
const root = ReactDOMClient.createRoot(container);

// Initial render: Render an element to the root.
root.render(
  <React.StrictMode>
    <BrowserRouter>
      <MoralisInitProvider>
        <UserContextProvider>
          <Provider store={store}>
            <Router />
          </Provider>
        </UserContextProvider>
      </MoralisInitProvider>
    </BrowserRouter>
  </React.StrictMode>
);
