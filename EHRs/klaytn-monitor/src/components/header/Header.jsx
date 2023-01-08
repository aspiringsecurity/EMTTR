import React, { useState, useContext } from 'react';
import { ToastContainer } from 'react-toastify';
import { Link } from "react-router-dom";
import 'react-toastify/dist/ReactToastify.css';


import "./Header.css";
import InfoContext from '../../context/infoContext';

const Header = (props) => {
    const [darkMode, setDarkMode] = useState(true)
    const ctx = useContext(InfoContext);
    const [searchInput, setSearchInput] = useState("");

    const updateSearchInput = event => {
        setSearchInput(event.target.value);
    }

    const searchAddressHandler = async(e) => {
        e.preventDefault()

        
        ctx.fetchUserData(searchInput);
        setSearchInput("")
    }

    const changeModeHandler = () => {
        setDarkMode(!darkMode);
        document.body.classList.toggle('light-mode');
        props.onChangeDarkMode();
    }

    const modeContent = darkMode ? <span onClick={changeModeHandler} className="material-icons-sharp">light_mode</span> : <span onClick={changeModeHandler} className="material-icons-sharp">dark_mode</span>;

    return (
    <div className='header'>
        <div className="logo">
            <Link to="/" className='link'>
                <span className='warning'>KLAYTN</span>
                <span>WATCH</span>
            </Link>
        </div>
        <div className="form-control">
            <span onClick={searchAddressHandler} className="material-icons-sharp">search</span>
            <form onSubmit={searchAddressHandler}>
                <input value={searchInput} onChange={updateSearchInput} type="text" name="search"  placeholder='Search Address...'/>
            </form>
        </div>
        <div className="mode-toggle">
            { modeContent }
        </div>
        <ToastContainer />
    </div>
    )
}

export default Header