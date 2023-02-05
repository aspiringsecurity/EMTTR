import React from "react";
import { Link } from "react-router-dom";
import { supportedChains } from "../api/constants";

const MintComponent = ({ isMinting, minted, txLink, error, mintNFT, dao }) => {
  const handleClick = async () => {
    await mintNFT();
  };

  return (
    <section className="text-gray-600 body-font relative mt-28 lg:mt-24">
      <div className="container px-5 py-24 mx-auto">
        <div className="flex flex-col text-center w-full mb-20">
          <h1 className="sm:text-3xl text-2xl font-medium title-font mb-6 md:mb-8 text-gray-900">
            Mint a {dao.title} NFT
          </h1>

          <h3 className="lg:w-2/3 max-w-lg mx-auto leading-relaxed text-base">
            Please switch to the{" "}
            <span className="font-bold">
              {supportedChains["0x" + dao?.chainId?.toString(16)]}
            </span>{" "}
            Network before miniting to your{" "}
            <span className="font-bold">Metamask</span> wallet.
          </h3>
        </div>
        <div className="lg:w-1/2 md:w-2/3 mx-auto">
          <div className="flex flex-wrap -m-2">
            <div className="p-2 w-full">
              <button
                onClick={handleClick}
                className="flex mx-auto text-white bg-gold border-0 py-2 px-8 focus:outline-none hover:bg-yellow-600 rounded text-lg"
              >
                {isMinting && (
                  <svg
                    role="status"
                    className="mr-2 -ml-1 mt-1 w-5 h-5 text-gray-200 animate-spin dark:text-gray-600 fill-brown"
                    viewBox="0 0 100 101"
                    fill="none"
                    xmlns="http://www.w3.org/2000/svg"
                  >
                    <path
                      d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
                      fill="currentColor"
                    />
                    <path
                      d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
                      fill="currentFill"
                    />
                  </svg>
                )}
                {isMinting ? "Minting" : "Mint"}
              </button>
              {minted && (
                <p className="p-2 w-full pt-8 text-center">
                  {error ? (
                    "Looks like something went wrong 😔."
                  ) : (
                    <>
                      Transaction at{" "}
                      <a
                        className="text-gold hover:text-yellow-600"
                        href={txLink}
                        target="_blank"
                      >
                        {txLink.slice(0, 40)}
                        ...
                      </a>
                    </>
                  )}
                </p>
              )}
            </div>
            <div className="p-2 w-full pt-4 mt-12 border-t border-gray-200 text-center">
              <Link to="/" className="text-gold font-serif text-lg">
                Tribunals
              </Link>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};

export default MintComponent;
