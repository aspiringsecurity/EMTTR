/* eslint-disable react-hooks/exhaustive-deps */

import { useEffect, useState } from "react";
import { Routes, Route, useNavigate } from "react-router-dom";
import ReactDOM from "react-dom";

import AddressInfo from "./components/addressInfo/AddressInfo";
import Header from "./components/header/Header";
import Footer from "./components/footer/Footer";

import Home from "./components/home/Home";

function App() {
  let navigate = useNavigate()
  const [isDark, setIsDark] = useState(true)
  const changeDarkModeHandler = () => {
    setIsDark(!isDark);
  }

  useEffect(() => {
    if(window.location.pathname !== '/'){
      navigate('/', {replace : true})
    }
  }, [])


  return (
    <main className="app">
      <div className="bg-effect">
        <div className="circle1"></div>
        <div className="circle2"></div>
        <div className="circle3"></div>
      </div>
      <div className="container">
        <Header onChangeDarkMode={changeDarkModeHandler} />
        <Routes>
              <Route path="/" element={<Home isDark={isDark} />}/>
              <Route path="/address" element={<AddressInfo />}/>
          </Routes>
          {ReactDOM.createPortal(<Footer />, document.querySelector('#portaled'))}
      </div>
    </main>
  );
}

export default App;
