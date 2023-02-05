import moment from "moment";
import React from "react";
import { Link } from "react-router-dom";
import Tag from "../Tag";

const ProposalCard = ({ proposal, dao }) => {
  const isClosed = moment(proposal.endDate).isBefore(moment());
  const daysLeft = () => {
    if (isClosed) return "Ended";
    return `${moment(proposal.endDate).diff(moment(), "days")} days left`;
  };
  return (
    <div className="w-full relative rounded md:rounded-xl md:border border border-gray-200 hover:border-gold  text-base transition-colors border-b first:border-t">
      <div className="leading-5 sm:leading-6">
        <Link
          to={`/${dao.slug}/proposal/${proposal._id}`}
          className="p-3 sm:p-4 block"
        >
          <div>
            <div className="mb-2 flex justify-between items-center">
              <div className="flex items-center space-x-1">
                <span className="inline-block align-middle leading-none">
                  <span className="flex shrink-0 items-center justify-center">
                    <img
                      src={dao.logo}
                      className="rounded-full bg-[color:var(--border-color)]"
                      alt={dao.title}
                      style={{
                        width: "28px",
                        height: "28px",
                        minWidth: "28px",
                      }}
                    />
                  </span>
                </span>
                <span className="!ml-2 hidden xs:block">{dao.title}</span>
                <span>by </span>
                <span>
                  <div>
                    <p className="flex flex-nowrap">
                      <span className="truncate font-semibold w-full">
                        {proposal.author}
                      </span>
                    </p>
                  </div>
                  <div
                    className="z-50 min-w-[300px] bg-skin-header-bg border border-skin-border rounded-xl shadow-lg"
                    style={{
                      display: "none",
                      position: "absolute",
                      inset: "auto auto 0px 0px",
                      margin: "0px",
                      transform: "translate3d(531px, 3752px, 0px",
                    }}
                  >
                    <div className="m-4 mb-0 text-center">
                      {/* <span className="flex shrink-0 items-center justify-center mb-4">
                        <img
                          src="https://stamp.fyi/avatar/eth:0x5BFCB4BE4d7B43437d5A0c57E908c048a4418390?s=128"
                          className="rounded-full bg-[color:var(--border-color)]"
                          style={{
                            width: "64px",
                            height: "64px",
                            minWidth: "64px",
                          }}
                        />
                      </span> */}
                      <h3 className="mt-3">{proposal.author}</h3>
                    </div>
                  </div>
                </span>
              </div>
              <Tag
                text={dao.creator ? isClosed ? "Closed" : "Open": "Test"}
                bg={isClosed ? "bg-cadet" : "bg-gold"}
              />
            </div>
            <h3 className="my-1 leading-7 break-words font-semibold text-lg">
              {proposal.title}
            </h3>
            <p className="break-words mb-2 text-sm sm:text-md">
              {proposal.desc}
            </p>
            <div>
              <span className="mt-2 flex space-x-1 items-center">
                <i
                  className="iconfont iconcheck1 text-green"
                  style={{ fontSize: "20px", lineHeight: "20px" }}
                ></i>
                <span className="text-gray-500 text-sm">{daysLeft()}</span>
              </span>
            </div>
          </div>
        </Link>
      </div>
    </div>
  );
};

export default ProposalCard;
