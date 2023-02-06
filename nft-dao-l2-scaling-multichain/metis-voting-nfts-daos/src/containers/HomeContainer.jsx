import React, { useEffect, useState } from "react";
import "./home.css";
import Navbar from "../components/Navbar";
import Hero from "../components/Hero";
import DaosFromYourNFTs from "../components/DaosFromYourNFTs";
import AllDaos from "../components/AllDaos";
import { useContext } from "react";
import { UserContext } from "../context/UserContext";
import { TribunalsContext } from "../context/TribunalsContext";
import CheckNFTs from "../api/checkNFTs";

/**
 *
 * doc: https://github.com/nft-api/nft-api#supported-blockchains
 *
 */

const HomeContainer = () => {
  const {
    handleAuthenticate,
    handleUDAuthenticate,
    handleCBAuthenticate,
    authenticated,
  } = useContext(UserContext);
  const { tribunals } = useContext(TribunalsContext);
  const [userDAOsMatch, setUserDAOsMatch] = useState([]);

  useEffect(() => {
    let isSubscribed = true;
    (async () => {
      if (authenticated && isSubscribed) {
        const nfts = await CheckNFTs();
        if (nfts?.length) {
          const matches = [];
          nfts.forEach((nft) => {
            const trib = tribunals.find(
              (trib) =>
                trib.contract_address?.toLowerCase() ===
                nft.token_address?.toLowerCase()
            );
            if (trib) matches.push(trib);
          });
          setUserDAOsMatch(matches);
        }
      }
    })();
    return () => {
      isSubscribed = false;
    };
  }, [authenticated]);

  return (
    <>
      <Navbar />
      <Hero
        handleAuthenticate={handleAuthenticate}
        // handleUAuthenticate={handleUDAuthenticate}
        handleCBAuthenticate={handleCBAuthenticate}
      />
      <DaosFromYourNFTs
        authenticated={authenticated}
        userDAOsMatch={userDAOsMatch}
      />
      <AllDaos tribunals={tribunals} />
    </>
  );
};

export default HomeContainer;
