import React from "react";
import Navbar from "../components/Navbar";
import NewTribunal from "../components/tribunalComponents/NewTribunal";
import { useParams } from "react-router-dom";

const TribunalContainer = () => {
  const { id } = useParams();

  return (
    <>
      <Navbar />
      <div id="content" className="pb-6 pt-28 lg:pt-24">
        <div className="md:px-4 max-w-7xl mx-auto px-8">
          <h1 className="mb-4 text-3xl font-serif text-gray-800">
            Create A Tribunal
          </h1>

          <div className="mt-20">
            <NewTribunal />
          </div>
        </div>
      </div>
    </>
  );
};

export default TribunalContainer;
