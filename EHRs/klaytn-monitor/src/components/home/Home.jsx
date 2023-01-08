import React from 'react';

import AboutKlay from '../../assets/the.png';
import BlackLogo from '../../assets/LogoBlack.png';
import WhiteLogo from '../../assets/LogoWhite.png';
import './Home.css';

const Home = (props) => {
    return (
        <div className='home'>
            <section className="img-logo">
                <img src={props.isDark ? WhiteLogo : BlackLogo} alt="Klaytn Logo" />
            </section>
            <div className="about-klaytn-watch">
                {/* <h3 className='warning'>Klaytn Watch</h3> */}
                <p>KlaytnWatch is a Basic Defi Dashboard that can be used to track all your Tokens Balance, Value, NFT Balance  and Floor Price.</p>
                <p><span className='success bold'>How to Use</span>, Paste your 0x.. Address in the Search Bar Above, Then Click Enter to Search and your account details will be displayed.</p>
            </div>
            <div className="about-klaytn">
                <h3 className='warning'>Klaytn BlockChain</h3>
                <p>
                Klaytn is a service-centric blockchain platform being developed by GroundX, the blockchain arm of Kakao, the company behind KakaoTalk, one of the most popular messaging apps in South Asia, with more than 50 million active users.
                </p>
                <p>Klaytn’s mainnet, Cypress, is already live and running, and boasts as much as 4,000 transactions per second (TPS). Transactions on the network have immediate finality and a new block is generated each second. Network security is being handled by up to 50 consensus nodes.</p>
                <p>The network also runs EVM (Ethereum Virtual Machine) and supports the execution of Solidity contracts, but ,unlike Ethereum, operates on a fixed gas price.</p>
                <div className="img">
                    <img src={AboutKlay} alt="About Klaytn" />
                </div>
            </div>
            <div className="about-covalent">
                <h3 className='warning'>Covalent</h3>
                <p>Covalent has a fully packed API, which fetches different types of Blockchain Data, and also works across different Blockchains, this website uses their API to fetch Tokens and NFTs data of Addresses on Klaytn Blockchain.</p>
            </div>
        </div>
    )
}

export default Home