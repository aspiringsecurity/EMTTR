import moment from "moment";
import React, { useEffect, useState } from "react";
import { truncateWithEllipsis } from "../../api/utils";

const VoteResults = ({ proposal }) => {
  const [results, setResults] = useState({ for: 0, against: 0 });

  const totalVotes = proposal?.votes?.ballot
    ? Object.keys(proposal.votes.ballot).length
    : 0;

  const percentVote = (v) =>
    proposal && totalVotes > 0 && v
      ? Math.round((v * 10) / totalVotes) / 10
      : 0;

  useEffect(() => {
    let isSubscribed = true;
    const votes = proposal?.votes?.ballot;
    const res = { for: 0, against: 0 };
    if (votes) {
      Object.keys(votes).map((key) => {
        if (votes[key].toLowerCase() == "for") res.for++;
        else res.against++;
      });
    }
    setResults(res);
    return () => {
      isSubscribed = false;
    };
  }, [proposal]);

  return (
    <div className="w-full lg:w-4/12 lg:min-w-[321px]" id="sidebar-right">
      <div className="space-y-4 mt-4 lg:mt-0">
        <div className="md:rounded-xl md:border bg-skin-block-bg border-skin-border text-base">
          <h4
            className="px-4 pt-3 block rounded-t-none md:rounded-t-lg border-y md:border-t-0 border-skin-border"
            style={{ paddingBottom: "12px" }}
          >
            Information{" "}
          </h4>
          <div className="p-4 leading-5 sm:leading-6">
            <div className="space-y-1">
              <div>
                <b>IPFS</b>
                <a
                  href={`https://w3s.link/ipfs/${proposal?.votes?.cid}`}
                  target="_blank"
                  className="whitespace-nowrap float-right text-gold"
                  rel="noopener noreferrer"
                  title={proposal?.votes?.cid}
                >
                  {" "}
                  #{proposal?.votes?.cid &&
                    truncateWithEllipsis(proposal?.votes.cid, 12, "end")}
                  <i
                    className="iconfont iconexternal-link ml-1"
                    style={{ fontSize: "16px", lineHeight: "16px" }}
                  ></i>
                </a>
              </div>
              <div>
                <b>Voting system</b>
                <span className="float-right text-skin-link">Basic voting</span>
              </div>
              <div>
                <b>Start date</b>
                <span className="float-right text-skin-link">
                  {moment(proposal?.startDate).format("MMM DD, YYYY, HH:mm")}
                </span>
              </div>
              <div>
                <b>End date</b>
                <span className="text-skin-link float-right">
                  {moment(proposal?.endDate).format("MMM DD, YYYY, HH:mm")}
                </span>
              </div>
              <div>
                <b>Snapshot</b>
                <a
                  href="https://etherscan.io/block/14394206"
                  target="_blank"
                  className="whitespace-nowrap float-right"
                  rel="noopener noreferrer"
                >
                  14,394,206
                  <i
                    className="iconfont iconexternal-link ml-1"
                    style={{ fontSize: "16px", lineHeight: "16px" }}
                  ></i>
                </a>
              </div>
            </div>
          </div>
        </div>
        {/* 
        * 
        *
        Results
        *
        *
        */}
        <div className="md:rounded-xl md:border bg-skin-block-bg border-skin-border text-base">
          <h4
            className="px-4 pt-3 block rounded-t-none md:rounded-t-lg border-y md:border-t-0 border-skin-border"
            style={{ paddingBottom: "12px" }}
          >
            Results
          </h4>
          <div className="p-4 leading-5 sm:leading-6">
            <div className="space-y-3">
              <div>
                <div className="text-skin-link mb-1 flex justify-between">
                  <div className="flex overflow-hidden">
                    <span className="mr-1 truncate">For: {results.for}</span>
                  </div>
                </div>
                <div className="h-2 relative overflow-hidden rounded-full flex">
                  <div className="w-full h-full bg-gray-200 absolute z-5"></div>
                  <div
                    className="bg-gold h-full z-10"
                    style={{
                      width: `${percentVote(results.for) * 100}%`,
                    }}
                  ></div>
                </div>
              </div>
              <div>
                <div className="text-skin-link mb-1 flex justify-between">
                  <div className="flex overflow-hidden">
                    <span className="mr-1 truncate">
                      Against: {results.against}
                    </span>
                  </div>
                </div>
                <div className="h-2 relative overflow-hidden rounded-full flex">
                  <div className="w-full h-full bg-gray-200 absolute z-5"></div>
                  <div
                    className="bg-cadet h-full z-10"
                    style={{
                      width: `${percentVote(results.against) * 100}%`,
                    }}
                  ></div>
                </div>
              </div>
              {/* <div>
                <div className="text-skin-link mb-1 flex justify-between">
                  <div className="flex overflow-hidden">
                    <span className="mr-1 truncate">Abstain</span>
                  </div>
                  <template className="flex justify-end space-x-2"></template>
                </div>
                <div className="h-2 relative overflow-hidden rounded-full flex">
                  <div className="w-full h-full bg-[color:var(--border-color)] absolute z-5"></div>
                  <div
                    className="bg-primary h-full z-10"
                    style={{ width: "0.499%" }}
                  ></div>
                </div>
              </div> */}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default VoteResults;
