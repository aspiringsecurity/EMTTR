import React, { useState } from "react";
import CheckNFTs, { VerifyNFTs } from "../api/checkNFTs";
import { ConnectWallet } from "../api/mintNFT";
import { updateProposal } from "../api/proposals";
import { uploadtoIPFS } from "../api/utils";

const CastVote = ({ proposal, handleProposal }) => {
  const [message, setMessage] = useState(null);
  const handleMessage = (m) => {
    setMessage(m);
    setTimeout(() => setMessage(null), 5000);
  };
  const handleVote = async (type) => {
    if (proposal) {
      try {
        const hasNFT = await VerifyNFTs();
        if (hasNFT) {
          const userAddress = await ConnectWallet();
          if (userAddress) {
            const voters = Object.keys(proposal.votes?.ballot || {});

            if (!voters.includes(userAddress)) {
              const cid = await uploadtoIPFS({
                ...proposal,
                votes: {
                  ballot: {
                    prevCid: proposal.votes?.cid || "none",
                    ...proposal.votes?.ballot,
                    [userAddress]: type,
                  },
                },
              });
              if (cid) {
                const data = await updateProposal(proposal._id, {
                  ...proposal,
                  votes: {
                    ballot: {
                      ...proposal.votes?.ballot,
                      [userAddress]: type,
                    },
                    cid,
                  },
                });

                if (data) {
                  handleMessage("Vote Sucessful.");
                  handleProposal(data);
                } else handleMessage("Something went wrong");
              } else handleMessage("Failed to upload to IPFS");
            } else handleMessage("This address has already Voted.");
          } else handleMessage("Couldn't verify your wallet.");
        } else handleMessage("No Matching NFTs for this DAO in your wallet.");
      } catch (err) {
        console.log(err);
        handleMessage(err.message);
      }
    }
  };
  return (
    <div className="md:rounded-xl md:border bg-skin-block-bg border-skin-border text-base my-4">
      <h4
        className="px-4 pt-3 block rounded-t-none md:rounded-t-lg border-y md:border-t-0 border-skin-border"
        style={{ paddingBottom: "12px" }}
      >
        Cast your vote{" "}
      </h4>
      <div className="p-4 leading-5 sm:leading-6">
        <div className="mb-3">
          <div className="mb-3">
            <button
              type="button"
              onClick={() => handleVote("For")}
              className="button px-[24px] py-2 block w-full mb-2 rounded md:rounded-xl md:border border active:bg-gold  border-gray-200 hover:border-gold"
            >
              For
            </button>
            <button
              type="button"
              onClick={() => handleVote("Against")}
              className="button px-[24px] py-2 mt-1 block w-full mb-2 rounded md:rounded-xl md:border border active:bg-gold  border-gray-200 hover:border-gold"
            >
              Against
            </button>
          </div>
        </div>
        {message && (
          <p
            className={`my-2 text-center ${
              message === "Vote Sucessful." ? "text-green-500" : "text-red-500"
            }`}
          >
            {message}
          </p>
        )}
      </div>
    </div>
  );
};

export default CastVote;
