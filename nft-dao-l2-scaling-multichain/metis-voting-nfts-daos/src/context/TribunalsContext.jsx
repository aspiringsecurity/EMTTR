import React, { useState, createContext, useEffect } from "react";
import { getTribunals } from "../api/tribunals";
import Daos from "../api/testDaos.json";
import { getImgUrl } from "../api/utils";

/**
 *
 * Gatsby Context API: https://medium.com/swlh/gatsbys-global-state-management-with-react-s-context-5f8064e93351
 *
 */

export const TribunalsContext = createContext({});

const TribunalsContextProvider = ({ children }) => {
  const [tribunals, setTribunals] = useState(Daos);

  useEffect(() => {
    (async () => {
      const data = await getTribunals();
      [].map;
      const updatedData = data.map((t) => ({ ...t, logo: getImgUrl(t.logo) }));

      setTribunals([...updatedData, ...Daos]);
    })();
  }, []);

  return (
    <TribunalsContext.Provider
      value={{
        tribunals,
      }}
    >
      {children}
    </TribunalsContext.Provider>
  );
};
export default TribunalsContextProvider;
