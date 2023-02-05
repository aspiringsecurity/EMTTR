import React from "react";
import MintContainer from "../containers/MintContainer";
import TribunalsContextProvider from "../context/TribunalsContext";

const MintPage = () => {
  return (
    <TribunalsContextProvider>
      <MintContainer />
    </TribunalsContextProvider>
  );
};

export default MintPage;
