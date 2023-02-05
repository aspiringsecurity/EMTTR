import React, { useContext, useEffect, useState } from "react";
import MintComponent from "../components/MintComponent";
import Navbar from "../components/Navbar";
import { useMintNftAction } from "../api/mintNFT";
import { TribunalsContext } from "../context/TribunalsContext";
import { useParams } from "react-router-dom";
import { scrollToTop } from "../api/utils";
import WorldCoinVerify from "../components/WorldCoin";
import { useCallback } from "react";

const MintContainer = () => {
  const { slug } = useParams();
  const { tribunals, isHuman, setIsHuman } = useContext(TribunalsContext);
  const [intentMint, setIntentMint] = useState(false);

  const [dao, setDao] = useState(null);
  const { isMinting, minted, txLink, error, mintNFT } = useMintNftAction(dao);

  useEffect(() => {
    scrollToTop();
    const dao = tribunals.find((daoObj) => daoObj.slug === slug);

    setDao(dao);
  }, [tribunals]);

  const handleMint = useCallback(() => {
    // ? Remove worldcoin verify for now.
    // if (isHuman) mintNFT();
    // setIntentMint(true);
    mintNFT(dao?.chainId);
    // }, [isHuman]);
  }, [dao]);

  // ? Remove worldcoin verify for now.

  // if (intentMint && !isHuman)
  //   return <WorldCoinVerify setIsHuman={setIsHuman} />;

  return (
    <>
      <Navbar />
      {dao && (
        <MintComponent
          isMinting={isMinting}
          minted={minted}
          txLink={txLink}
          error={error}
          mintNFT={handleMint}
          dao={dao}
        />
      )}
    </>
  );
};

export default MintContainer;
