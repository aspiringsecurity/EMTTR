import Head from "next/head";
import Image from "next/image";
import CreateForm from "../components/CreateForm";

import YourCrews from "./../components/YourCrews";
const Home = (props) => {
  const { isAuthenticated } = props;

  return (
    <div>
      <Head>
        <title>CREWS DataDAO</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      {!isAuthenticated ? (
        <>
          <div className="h-fit w-full lg:w-1/2 xl:w-1/4 absolute right-2 sm:right-32 top-3 sm:top-20 z-100 text-white flex flex-col gap-5 sm:gap-6 bg-black/50 backdrop-blur-md rounded-lg p-3 sm:p-10 ">
            <div className="ml-16 sm:ml-0 text-3xl md:text-4xl xl:text-6xl text-tertiary bold md:mb-10">
              CrewS DataDAO
            </div>
            <p className="text-white text-xl font-semibold">
              CrewS is a No Code Tool to create Data DAOs. With CrewS you can
              take your crew on chain quickly to manage data along with
              governance.
            </p>
          </div>
        </>
      ) : (
        <div className="h-full w-full absolute top-0 z-100 text-white flex flex-col justify-center items-center gap-5 sm:gap-6 rounded-lg p-3 sm:p-16 bg-quaternary/50">
          <YourCrews />
        </div>
      )}
      <footer className="bg-quaternary w-full h-fit sticky z-50 bottom-0">
        <div className="max-w-7xl mx-auto p-1 overflow-hidden">
          <h1 className="text-xl text-white">Made with ❤️ for EthIndia</h1>
        </div>
      </footer>
    </div>
  );
};

export default Home;

// fdeab1
