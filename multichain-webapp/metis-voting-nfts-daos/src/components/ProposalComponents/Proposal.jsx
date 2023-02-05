import moment from "moment";
import React, { useEffect, useState } from "react";
import { Link, useParams } from "react-router-dom";
import { getProposal } from "../../api/proposals";
import { truncateWithEllipsis } from "../../api/utils";
import { ChatProvider } from "../../context/chatContext";
import CastVote from "../CastVote";
import Discussion from "../Discussion";
import Tag from "../Tag";
import ProposalSkeleton from "./ProposalSkeleton";
import VoteResults from "./VoteResults";
import VotesList from "./VotesList";

const Proposal = ({ _proposal, dao }) => {
  const { id } = useParams();
  const [proposal, setProposal] = useState({});
  const isClosed = (p) => moment(p).isBefore(moment());

  const handleProposal = (p) => setProposal(p);

  useEffect(() => {
    let isSubscribed = true;

    if (isSubscribed && !_proposal)
      (async () => {
        const data = await getProposal(id);
        if (data) handleProposal(data);
      })();
    return () => {
      isSubscribed = false;
    };
  }, []);

  return (
    <ChatProvider>
      <div className="lg:flex px-2 sm:px-5 md:px-0 lg:ml-72">
        <div
          className="w-full lg:w-8/12 lg:pr-5 md:mt-3 relative"
          id="content-left"
        >
          <div className="w-full px-3 md:px-0 mb-2 flex justify-between items-center">
            <div className="flex items-center flex-auto">
              <h2 className="font-serif text-xl">Proposal</h2>
            </div>
            <Link
              to={`/${dao.slug}/proposals`}
              className="flex items-center flex-auto justify-end"
            >
              <svg
                xmlns="http://www.w3.org/2000/svg"
                className="icon icon-tabler icon-tabler-arrow-narrow-left"
                width="24"
                height="24"
                viewBox="0 0 24 24"
                strokeWidth="1.5"
                stroke="#2c3e50"
                fill="none"
                strokeLinecap="round"
                strokeLinejoin="round"
              >
                <path stroke="none" d="M0 0h24v24H0z" fill="none" />
                <line x1="5" y1="12" x2="19" y2="12" />
                <line x1="5" y1="12" x2="9" y2="16" />
                <line x1="5" y1="12" x2="9" y2="8" />
              </svg>
              <h2 className="text-sm">Back</h2>
            </Link>
          </div>

          {Object.keys(proposal).length > 0 ? (
            <>
              <div className="px-3 md:px-0">
                <h1 className="mb-3 break-words text-xl leading-8 sm:text-2xl">
                  {proposal.title}
                </h1>
                <div className="flex flex-col sm:flex-row sm:space-x-1 mb-4">
                  <div className="flex items-center mb-1 sm:mb-0">
                    <Tag
                      text={isClosed(proposal.endDate) ? "Closed" : "Open"}
                      bg={isClosed(proposal.endDate) ? "bg-cadet" : "bg-gold"}
                      noAbsolute
                    />
                    <a
                      href="#/ens.eth"
                      className="router-link-active text-skin-text group"
                    >
                      <div className="flex items-center">
                        <span className="inline-block align-middle leading-none">
                          <span className="flex shrink-0 items-center justify-center">
                            <img
                              src={dao.logo}
                              className="rounded-full bg-[color:var(--border-color)]"
                              alt="ENS"
                              style={{
                                width: "28px",
                                height: "28px",
                                minWidth: "28px",
                              }}
                            />
                          </span>
                        </span>
                        <span className="ml-2 group-hover:text-skin-link">
                          Proposal
                        </span>
                      </div>
                    </a>
                  </div>
                  <div className="flex grow items-center space-x-1">
                    <span>by </span>
                    <span>{truncateWithEllipsis(proposal.author, 8)}</span>

                    <div className="relative inline-block text-left h-full md:ml-2">
                      <div className="inline-flex items-center w-full h-full cursor-pointer">
                        <div className="pl-1">
                          <i
                            className="iconfont iconthreedots hover:text-skin-link"
                            style={{ fontSize: "25px", lineHeight: "25px" }}
                          ></i>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div className="relative">{proposal.desc}</div>
              </div>
              <CastVote proposal={proposal} handleProposal={handleProposal} />
              <Discussion proposal={proposal} />
              <VotesList proposal={proposal} />
            </>
          ) : (
            <ProposalSkeleton />
          )}
        </div>
        {proposal && <VoteResults proposal={proposal} />}
      </div>
    </ChatProvider>
  );
};

export default Proposal;
