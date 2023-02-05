import moment from "moment";
import React from "react";
import { useEffect } from "react";
import { useState } from "react";
import { truncateWithEllipsis } from "../../api/utils";
import ProposalCard from "./ProposalCard";
import ProposalCardskeleton from "./ProposalCardskeleton";

const ProposalsList = ({ proposals, dao }) => {
  const [filteredProposals, setFilteredProposals] = useState();
  const daysLeft = (date) => moment(date).diff(moment(), "days");
  const sortFunction = (a, b) => {
    return daysLeft(a.endDate) - daysLeft(b.endDate);
  };

  const sortProposals = (proposals) => {
    const closed = [];
    const open = [];
    proposals.forEach((proposal) => {
      moment(proposal.endDate).isBefore(moment())
        ? closed.push(proposal)
        : open.push(proposal);
      proposal.author = truncateWithEllipsis(proposal.author, 8);
    });
    closed.sort(sortFunction);
    open.sort(sortFunction);
    return [...open, ...closed];
  };

  useEffect(() => {
    if (dao.creator) {
      const sortedProposals = sortProposals(
        proposals.filter((p) => p.tribunalId === dao._id)
      );
      setFilteredProposals(sortedProposals);
    } else {
      const sortedProposals = sortProposals(proposals);
      setFilteredProposals(sortedProposals);
    }
  }, [proposals]);

  const Placeholder = () => (
    <>
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
      <ProposalCardskeleton />
    </>
  );

  return (
    <div className="w-full lg:w-3/4 lg:ml-72 relative" id="content-right">
      <div className="px-5 lg:px-0 mb-3 flex relative">
        <div className="flex-auto">
          <div className="flex items-center flex-auto">
            <h2 className="font-serif text-xl">Proposals</h2>
          </div>
        </div>
        <div className="relative inline-block text-left h-full">
          <div className="inline-flex items-center w-full h-full cursor-pointer">
            {/* //TODO: Filter List of proposals */}
            {/* <button
              type="button"
              className="button px-[24px] pr-3"
            >
              All{" "}
              <i
                className="iconfont iconarrow-down mt-1 mr-1"
                style={{ fontSize: "14px", lineHeight: "14px" }}
              ></i>
            </button> */}
          </div>
        </div>
      </div>
      <div className="space-y-4 px-5 lg:px-0 my-4 lg:w-11/12 w-full">
        {filteredProposals?.length > 0 ? (
          filteredProposals.map((proposal) => (
            <ProposalCard key={proposal._id} proposal={proposal} dao={dao} />
          ))
        ) : proposals.length > 10 ? (
          <p className="w-full text-center mt-24">No proposals yet</p>
        ) : (
          <Placeholder />
        )}
      </div>
      <div className="w-[10px] h-[10px] absolute bottom-0"></div>
    </div>
  );
};

export default ProposalsList;
