import React from "react";
import { useState, useEffect } from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faHammer, faList, faUpLong } from "@fortawesome/free-solid-svg-icons";
import { useSmartContract } from "../hooks/useSmartContract";

const ProposalStore = () => {
  const { getDataDaoMetadata, createDataDao } = useSmartContract();

  // fetch posts from API in a array hook
  // TODO : fetch from API
  // const proposals = useFetch('https://api.thegraph.com/subgraphs/name/1hive/uniswap-v2', {}

  // make the call
  //   const [proposals, setProposals] = useState([
  //     {
  //       id: 1,
  //       title: "Proposal 1",
  //       description: "Proposal 1 description",
  //       votes: 100,
  //       status: "Active",
  //       creator: "0x1234567890",
  //       createdAt: "2021-01-01",
  //       link: "https://google.com",
  //     },
  //     {
  //       id: 2,
  //       title: "Proposal 2",
  //       description: "Proposal 2 description",
  //       votes: 200,
  //       status: "Active",
  //       creator: "0x1234567890",
  //       createdAt: "2020-11-01",
  //       link: "https://google.com",
  //     },
  //   ]);

  const [proposals, setProposals] = useState([]);

  // add useEffect to call getDataDao
  useEffect(() => {
    getDataDaoMetadata().then((data) => {
      setProposals(data);
    });
  }, []);

  return (
    <div className="w-10/12 h-full flex flex-col justify-start items-start gap-5 px-5">
      {proposals?.map((proposal) => {
        return (
          <div
            className="w-full h-full flex flex-col justify-start items-start flex-wrap gap-3"
            key={proposal.id}
          >
            <h1 className="text-sm text-gray-300">{proposal.createdAt}</h1>
            <div className="w-full h-full flex flex-col justify-evenly items-center bg-black border-2 border-secondary/30 rounded-md p-3 gap-2">
              <div className="w-full h-full grid grid-cols-4">
                <h1 className="flex justify-start px-2 items-center col-span-2">
                  {proposal.title}
                </h1>
                <h1 className="flex justify-start px-2 items-center col-span-1">
                  {proposal.status}
                </h1>
                <h1 className="flex justify-start px-2 items-center col-span-1">
                  {proposal.creator}
                </h1>
              </div>
              <div className="w-full h-full grid grid-cols-4">
                <div className="flex justify-start px-2 items-center col-span-1 gap-2">
                  <FontAwesomeIcon
                    icon={faUpLong}
                    className="h-5 w-5 text-gray-300"
                  />
                  <h1>{proposal.votes}</h1>
                </div>
                <h1 className="flex justify-start px-2 items-center col-span-3">
                  {proposal.description}
                </h1>
              </div>
            </div>
          </div>
        );
      })}
    </div>
  );
};

const CreateProposal = () => {
  const [proposal, setProposal] = useState("");
  const { getDataDaoMetadata, createDataDao } = useSmartContract();
  const someFx = () => {
    console.log("Do nothing");
  };
  return (
    <div className="w-10/12 h-full flex flex-col justify-start items-start gap-5 px-5">
      <h1 className="text-white text-5xl">Proposal Builder</h1>
      <h1 className="text-2xl text-white">Name your Proposal</h1>
      <div className="user-box w-full md:w-1/3">
        <div className="relative inline-block px-4 py-2 font-medium group w-full sm:w-96">
          <span className="absolute rounded-lg inset-0 w-full h-full transition duration-200 ease-out transform translate-x-1 translate-y-1 bg-tertiary border-[2px] border-black group-hover:-translate-x-0 group-hover:-translate-y-0"></span>
          <span className="absolute rounded-lg inset-0 w-full h-full bg-white border-2 border-black group-hover:bg-tertiary"></span>
          <textarea
            type="text"
            name="user_name"
            className="w-full px-8 rounded text-black relative"
            required
            onInput={(e) => setProposal(e.target.value)}
          ></textarea>
          <label className="px-5">{"Proposal Title*"}</label>
        </div>
      </div>
      <div className="w-full h-full flex flex-col sm:flex-row justify-end items-center">
        <div className="relative inline-block px-4 py-2 font-medium group w-full sm:w-96">
          <span className="absolute rounded-lg inset-0 w-full h-full transition duration-200 ease-out transform translate-x-1 translate-y-1 bg-tertiary border-[2px] border-black group-hover:-translate-x-0 group-hover:-translate-y-0"></span>
          <span className="absolute rounded-lg inset-0 w-full h-full bg-secondary border-2 border-black group-hover:bg-tertiary"></span>
          <button
            className="relative text-black text-xl font-semibold"
            onClick={() => someFx()}
          >
            Deploy Proposal!
          </button>
        </div>
      </div>
    </div>
  );
};

const DAO = (props) => {
  const { selectedDAO } = props; // id of DAO in question
  const [activeTab, setActiveTab] = useState(0);
  return (
    <div className="w-screen h-screen flex justify-center items-start">
      <div className="w-full h-fit sm:w-10/12 sm:h-10/12 flex justify-evenly items-center mt-20 sm:mt-0 backdrop-blur-md bg-quaternary/60 rounded-xl text-white border border-secondary/40 hover:border-secondary/70">
        <div className="w-full h-full flex flex-col sm:flex-row justify-evenly items-start py-10 text-xl gap-3">
          <div className="w-full sm:w-2/12 h-full flex flex-row sm:flex-col justify-start items-start border-r-2 border-primary/40 sm:mx-2 gap-5">
            <div className="w-full h-fit flex flex-col sm:flex-row justify-start items-center bg-cyan-500/30 p-3 gap-3">
              <img
                class="p-1 w-12 h-12 rounded-full ring-2 ring-gray-300 dark:ring-gray-500"
                src="/pic1.png"
                alt="Bordered avatar"
              />
              <div className="w-full h-full flex flex-row sm:flex-col justify-evenly items-center">
                <h1 className="text-white text-sm">0x1234567890</h1>
                <h1 className="text-white text-sm">$ 5.76</h1>
              </div>
            </div>
            <ul className="w-full">
              <li
                className="w-full flex justify-start gap-2 items-center p-2 text-base font-normal text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700"
                onClick={() => setActiveTab(0)}
              >
                <FontAwesomeIcon
                  icon={faHammer}
                  className="h-5 w-5 text-gray-300"
                />
                <h1>Create Proposal</h1>
              </li>
              <li
                className="w-full flex justify-start gap-2 items-center p-2 text-base font-normal text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700"
                onClick={() => setActiveTab(1)}
              >
                <FontAwesomeIcon
                  icon={faList}
                  className="h-5 w-5 text-gray-300"
                />
                <h1>Proposals</h1>
              </li>
            </ul>
          </div>
          {activeTab === 0 ? <CreateProposal /> : <ProposalStore />}
        </div>
      </div>
    </div>
  );
};

export default DAO;
