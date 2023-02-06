import React, { useState, createContext, useEffect } from "react";
import { uauth } from "../api/unstoppableAuth";
import {
  mmAuthenticate,
  cbAuthenticate,
  scrollToTop,
  truncateWithEllipsis,
} from "../api/utils";

export const UserContext = createContext({});
// const appId = import.meta.env.VITE_MORALIS_APP_ID;
// const serverUrl = import.meta.env.VITE_MORALIS_SERVER_URL;

const UserContextProvider = ({ children }) => {
  const [user, setUser] = useState(null);
  const [mUser, setMUser] = useState(null); // Moralis User
  const [udUser, setUdUser] = useState(null);
  const [isHuman, setIsHuman] = useState(false);

  const handleAuthenticate = async () => {
    try {
      const data = await mmAuthenticate({
        signingMessage: "Sign in to Tribunals",
      });
      setMUser(data);
    } catch (err) {
      console.log(err);
    }
  };

  const handleUDAuthenticate = async () => {
    try {
      const data = await Moralis.authenticate(uauth);
      setMUser(data);
    } catch (err) {
      console.log(err);
    }
  };

  const handleCBAuthenticate = async () => {
    try {
      const data = await cbAuthenticate();
      // const data = await Moralis.authenticate(uauth);
      setMUser(data);
    } catch (err) {
      console.log(err);
    }
  };

  const getUDUser = async (u) => {
    let user = {};
    try {
      const uauthMoralisConnector = new uauth.connector();

      user = { ...(await uauthMoralisConnector.uauth.user()) };
    } catch (e) {
      if (
        e.message === "Must import UAuth before constructing a UAuth Object"
      ) {
        console.warn(e);
      } else console.error(e);
    }
    if (user?.sub) {
      //Unstoppable signIn
      setUdUser(user);
      setUser({
        name: user.sub,
        address: user.wallet_address,
      });
    }
    if (u?.attributes?.ethAddress) {
      // Metamask signIn
      setUser({
        name: truncateWithEllipsis(u.attributes.ethAddress, 8),
        address: u.attributes.ethAddress,
      });
    } else {
      // CB SignIn
      setUser({
        name: truncateWithEllipsis(u.address, 8),
        address: u.address,
      });
    }
  };

  const signOut = () => {
    setUdUser(null);
    setUser(null);
    setMUser(null);
    scrollToTop();
  };

  const authenticated = !!user;

  useEffect(() => {
    if (mUser) getUDUser(mUser);
  }, [mUser]);

  return (
    <UserContext.Provider
      value={{
        mUser,
        udUser,
        user,
        signOut,
        authenticated,
        handleAuthenticate,
        handleUDAuthenticate,
        handleCBAuthenticate,
        getUDUser,
        isHuman,
        setIsHuman,
      }}
    >
      {children}
    </UserContext.Provider>
  );
};
export default UserContextProvider;
