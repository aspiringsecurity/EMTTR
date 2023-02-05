import React from "react";
import Metamask from "../assets/svg/metamask.svg?component"; 
import Unstoppable from "../assets/svg/unstoppable.svg?component";
import Coinbase from "../assets/svg/coinbase.svg?component";

const ConnectWallet = ({
  handleAuthenticate,
  handleUAuthenticate,
  handleCBAuthenticate,
}) => {
  return (
    <section id="sign-in" className="text-gray-600 body-font">
      <div className="container max-w-xl py-12 mx-auto">
        <div className="w-full flex flex-col sm:flex-row items-center mx-auto">
          <h3 className="flex-grow text-center text-base title-font text-gray-600 mb-5">
            Connect your wallet to filter DAOs by your NFTs.
          </h3>
        </div>
        <div className="flex w-full justify-center">
          <button
            onClick={handleCBAuthenticate}
            className="flex-shrink-0 flex text-cadet hover:bg-blue-700 hover:bg-opacity-5 border-blue-700 border py-2 px-4 focus:outline-none rounded text-lg mt-10 sm:mt-0 mr-2"
          >
            <Coinbase className="w-6" />
          </button>
          <button
            onClick={handleAuthenticate}
            className="flex-shrink-0 flex text-cadet hover:bg-gold hover:bg-opacity-5 border-gold border py-2 px-4 focus:outline-none rounded text-lg mt-10 sm:mt-0 mx-2"
          >
            <Metamask className="w-[22px] self-center" />
          </button>
          {/* <button
            onClick={handleUAuthenticate}
            className="flex-shrink-0 flex text-cadet hover:bg-blue-600 hover:bg-opacity-5 border-blue-600 border py-2 px-4 focus:outline-none rounded text-lg mt-10 sm:mt-0 ml-2"
          >
            <Unstoppable className="w-6" />
          </button> */}
        </div>
      </div>
    </section>
  );
};

export default ConnectWallet;
