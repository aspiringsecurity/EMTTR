import React, { createContext } from "react";

export const MoralisInitContext = createContext({});
const apiKey = import.meta.env.VITE_VITE_MORALIS_API_KEY;
const MoralisInitProvider = ({ children }) => {
  // Moralis.start({ apiKey });

  return (
    <MoralisInitContext.Provider value="">
      {children}
    </MoralisInitContext.Provider>
  );
};
export default MoralisInitProvider;
