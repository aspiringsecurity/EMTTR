import moment from "moment";
import React from "react";
import { Link } from "react-router-dom";

const AboutComponent = ({ dao }) => {
  return (
    <div
      className="w-full lg:w-3/4 lg:ml-72 max-w-3xl md:pr-5 relative"
      id="content-right"
    >
      <div className="w-full px-5 lg:px-0 mb-2 flex justify-between items-center">
        <div className="flex items-center flex-auto">
          <h2 className="font-serif text-xl">New Proposals</h2>
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
      <div className="md:space-y-4 px-5 lg:px-0 my-4">
        <div className="rounded-xl -ml-[1px] border text-base mb-3">
          <div className="p-4 leading-5 sm:leading-6">
            <div className="mb-3">
              <h4 className="text-skin-link mb-2 leading-7 break-words font-semibold text-lg">
                Network
              </h4>
              <div>Ropsten Testnet</div>
            </div>
            <div className="mb-3">
              <h4 className="text-skin-link mb-2 leading-7 break-words font-semibold text-lg">
                Description
              </h4>{" "}
              {dao.desc}
            </div>
            {dao.creator && (
              <>
                <div className="mb-3">
                  <h4 className="text-skin-link mb-2 leading-7 break-words font-semibold text-lg">
                    Started by
                  </h4>{" "}
                  {dao.creatorName}
                </div>
                <div className="mb-3">
                  <h4 className="text-skin-link mb-2 leading-7 break-words font-semibold text-lg">
                    Started at
                  </h4>{" "}
                  {moment(dao.createdAt).format("MMMM, YYYY")}
                </div>
              </>
            )}

            <div className="last:mb-0 mb-3"></div>
          </div>
        </div>
      </div>
      <div className="w-[10px] h-[10px] absolute bottom-0"></div>
    </div>
  );
};

export default AboutComponent;
