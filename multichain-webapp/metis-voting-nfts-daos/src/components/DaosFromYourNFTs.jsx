import React from "react";
import { Link } from "react-router-dom";
import DaosList from "./DaosList";

const DaosFromYourNFTs = ({ authenticated, userDAOsMatch }) => {
  return (
    <>
      <section className="text-gray-600 body-font">
        <div className="container max-w-7xl px-5 pt-24 mx-auto">
          <div className="flex flex-wrap w-full mb-20">
            <div className="lg:w-1/2 w-full mb-6 lg:mb-0">
              <h1 className="sm:text-3xl text-2xl font-serif font-medium title-font mb-2 text-gray-900">
                Tribunals From Your NFTs
              </h1>
              <div className="h-1 w-44 sm:w-60 bg-gold rounded"></div>
            </div>
          </div>
        </div>
      </section>
      {userDAOsMatch?.length > 0 ? (
        <DaosList userDAOsMatch={userDAOsMatch} />
      ) : (
        <div className="w-full max-w-lg text-center flex justify-center my-6 mx-auto">
          <h1 className="text-base sm:text-base px-2 font-mono  mb-2 text-gray-400">
            {authenticated
              ? "No Tribunal Matched Your NFTs."
              : "When You Connect Your Wallet, Tribunals That Match Your NFTs Will Be Displayed Here."}{" "}
            <p className="text-gold">
              Join any Tribunal by Minting their NFT.
            </p>
          </h1>
        </div>
      )}
    </>
  );
};

export default DaosFromYourNFTs;
