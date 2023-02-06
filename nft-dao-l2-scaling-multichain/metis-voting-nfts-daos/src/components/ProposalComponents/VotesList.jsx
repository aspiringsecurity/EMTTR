import React from "react";
import { truncateWithEllipsis } from "../../api/utils";

const VoteItem = ({ address, type }) => (
  <div className="px-3 py-3 border-t flex">
    <span className="w-[110px] xs:w-[130px] min-w-[110px] xs:min-w-[130px]">
      <div>
        <a className="flex flex-nowrap">
          <span className="flex shrink-0 items-center justify-center mr-2">
            <img
              src={`https://i.pravatar.cc/36?img=${Math.floor(
                Math.random() * 65
              )}`}
              className="rounded-full bg-[color:var(--border-color)]"
              style={{
                width: "18px",
                height: "18px",
                minWidth: "18px",
              }}
            />
          </span>
          <span className="w-full">
            {truncateWithEllipsis(address, 8)}
          </span>
        </a>
      </div>
    </span>
    <div className="flex-auto text-center text-skin-link truncate px-2">
      <div className="text-center text-skin-link capitalize">{type}</div>
    </div>
  </div>
);

const VotesList = ({ proposal }) => {
  const dBallot = proposal?.votes?.ballot;
  return (
    <div className="space-y-4 py-4">
      <div className="md:rounded-xl md:border bg-skin-block-bg border-skin-border text-base">
        <h4
          className="px-4 pt-3 block rounded-t-none md:rounded-t-lg border-y md:border-t-0 border-skin-border"
          style={{ paddingBottom: "12px" }}
        >
          Votes{" "}
          <div className="h-[20px] min-w-[20px] rounded-full leading-normal text-xs text-white bg-skin-text text-center px-1 ml-1 inline-block">
            445
          </div>
        </h4>
        <div className="leading-5 sm:leading-6">
          {dBallot &&
            Object.keys(dBallot).map((key) => (
              <VoteItem
                key={key}
                name={key}
                address={proposal?.author}
                type={dBallot[key]}
              />
            ))}
        </div>
      </div>
    </div>
  );
};

export default VotesList;
