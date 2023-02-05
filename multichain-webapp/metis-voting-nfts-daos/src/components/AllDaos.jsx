import React from "react";
import DaosList from "./DaosList";

const AllDaos = ({tribunals}) => {
  return (
    <>
      <section className="text-gray-600 body-font">
        <div className="container max-w-7xl px-5 pt-24 mx-auto">
          <div className="flex flex-wrap w-full mb-20">
            <div className="lg:w-1/2 w-full mb-6 lg:mb-0">
              <h1 className="sm:text-3xl text-2xl font-serif font-medium title-font mb-2 text-gray-900">
                All Tribunals
              </h1>
              <div className="h-1 w-20 sm:w-32 bg-gold rounded"></div>
            </div>
          </div>
        </div>
      </section>
      <DaosList tribunals={tribunals} />
    </>
  );
};

export default AllDaos;
