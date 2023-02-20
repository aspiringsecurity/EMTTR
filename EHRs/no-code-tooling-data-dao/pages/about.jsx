import Head from "next/head";
import Image from "next/image";
import CreateForm from "../components/CreateForm";

import SplineObj from "../components/SplineObj";
import YourCrews from "./../components/YourCrews";
const About = (props) => {
  return (
    <div>
      <Head>
        <title>CREWS DataDAO</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <div className="h-full w-full absolute top-0 z-100 text-white flex flex-col justify-center items-center gap-5 sm:gap-6 rounded-lg p-3 sm:p-20 bg-quaternary/50">
        <div className="w-full h-full flex flex-col justify-start items-center bg-quaternary/60 gap-5 p-8 rounded-2xl">
          <div className="ml-16 sm:ml-0 text-3xl md:text-4xl xl:text-6xl text-tertiary font-bold md:mb-10">
            CrewS DataDAO
          </div>
          <p className="text-white text-xl font-semibold">
            CrewS is a No Code Tool to create Data DAOs. With CrewS you can take
            your crew on chain quickly to manage data along with governance. ##
            Features - Managing Proposals - OnChain Voting - Execution of
            proposals - Storing data as per the voting results
          </p>
        </div>
      </div>
      <footer className="bg-quaternary w-full h-fit sticky z-50 bottom-0">
        <div className="max-w-7xl mx-auto p-1 overflow-hidden">
          <h1 className="text-xl text-white">Made with ❤️ for EthIndia</h1>
        </div>
      </footer>
    </div>
  );
};

export default About;

// fdeab1
