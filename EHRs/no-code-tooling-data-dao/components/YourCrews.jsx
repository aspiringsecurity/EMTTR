import React from "react";
import { useState, useEffect } from "react";
import CreateForm from "./CreateForm";
import DAO from "./DAO";
import { useSmartContract } from "../hooks/useSmartContract";

const YourCrews = () => {
  const [selectedDAO, setSelectedDAO] = useState(0);
  const { getDataDao } = useSmartContract();
  const [DAOviewer, setDAOviewer] = useState(false);
  const [makeNewCrew, setMakeNewCrew] = useState(false);
  //   const [crews, setCrews] = useState([]);

  //   useEffect(() => {
  //     getDataDao().then((data) => {
  //       setCrews(data);
  //     });
  //   }, []);

  const [crews, setCrews] = useState([
    {
      id: 1,
      name: "Crew 1",
      description: "Crew 1 description",
      members: 100,
      status: "Active",
      creator: "0x1234567890",
      createdAt: "2021-01-01",
      link: "https://google.com",
    },
    {
      id: 2,
      name: "Crew 2",
      description: "Crew 2 description",
      members: 200,
      status: "Active",
      creator: "0x1234567890",
      createdAt: "2020-11-01",
    },
  ]);
  return (
    <>
      {!DAOviewer ? (
        <>
          {!makeNewCrew ? (
            <div className="w-screen h-screen flex justify-center items-start">
              <div className="w-full h-fit sm:w-10/12 sm:h-10/12 flex justify-evenly items-center mt-20 sm:mt-0 backdrop-blur-md bg-quaternary/60 rounded-xl text-white border border-secondary/40 hover:border-secondary/70">
                <div
                  id="form"
                  className="w-full h-full flex flex-col justify-evenly items-center p-2 sm:p-5"
                >
                  <div className="w-full h-full flex flex-col sm:flex-row justify-evenly items-center">
                    <h1 className="text-2xl sm:text-3xl md:text-5xl font-semibold text-white font-raleway">
                      Yo' Crews
                    </h1>
                    <div className="relative inline-block px-4 py-2 font-medium group w-full sm:w-96">
                      <span className="absolute rounded-lg inset-0 w-full h-full transition duration-200 ease-out transform translate-x-1 translate-y-1 bg-tertiary border-[2px] border-black group-hover:-translate-x-0 group-hover:-translate-y-0"></span>
                      <span className="absolute rounded-lg inset-0 w-full h-full bg-secondary border-2 border-black group-hover:bg-tertiary"></span>
                      <button
                        className="relative text-black text-xl font-semibold"
                        onClick={() => setMakeNewCrew(true)}
                      >
                        Create a new DAO
                      </button>
                    </div>
                  </div>
                  <div className="w-full h-full flex flex-col justify-start items-start p-1 sm:p-10 gap-2 sm:gap-5">
                    {crews.map((crew) => {
                      return (
                        <div
                          className="w-full h-full flex flex-col sm:flex-row justify-start items-start flex-wrap text-xl bg-black/70 rounded-full p-5 border border-primary/80 gap-2 sm:gap-5"
                          key={crew.id}
                          onClick={() => {
                            setSelectedDAO(crew.id);
                            setDAOviewer(true);
                          }}
                        >
                          <h1 className="text-gray-300">{crew.id}.</h1>
                          <h1 className="text-white">{crew.name}</h1>
                        </div>
                      );
                    })}
                  </div>
                </div>
              </div>
            </div>
          ) : (
            <CreateForm setMakeNewCrew={setMakeNewCrew} />
          )}
        </>
      ) : (
        <DAO selectedDAO={selectedDAO} />
      )}
    </>
  );
};

export default YourCrews;
