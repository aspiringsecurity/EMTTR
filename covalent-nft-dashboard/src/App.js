import React, { useState } from 'react';
import { Layout } from 'antd';
import { HashRouter, Route, Routes } from 'react-router-dom';

import './App.css';
import Navbar from './components/layout/Navbar';
import Dashboard from './pages/Dashboard';
import ContractDetail from './pages/ContractDetail';

function App() {
  const [type, setType] = useState("137");
  const [userNFTs, setUserNFTs] = useState([]);
  const [nftLoading, setNFTLoading] = useState(false);
  const [chainIconURL, setChainIconURL] = useState("");

  return (
    <HashRouter>
      <Layout className="layout">
        <Navbar
          type={type}
          setUserNFTs={setUserNFTs}
          setNFTLoading={setNFTLoading}
          setType={setType}
          setChainIconURL={setChainIconURL} />
        <Layout.Content className="main-layout">
          <Routes>
            <Route
              path="/contract/:contractaddress"
              element={
                <ContractDetail type={type} />} />
            <Route
              path="/"
              element={
                <Dashboard
                  userNFTs={userNFTs}
                  nftLoading={nftLoading}
                  chainIconURL={chainIconURL} />} />
          </Routes>
        </Layout.Content>
      </Layout>
    </HashRouter>
  );
}

export default App;
