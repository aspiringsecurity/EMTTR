import React from 'react';
import ReactDOM from 'react-dom/client';
import { BrowserRouter } from "react-router-dom";

import './index.css';
import App from './App';
import InfoContextProvider from './context/infoContextProvider/InfoContextProvider';

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
    <BrowserRouter>
        <InfoContextProvider>
            <App />
        </InfoContextProvider>
    </BrowserRouter>
);