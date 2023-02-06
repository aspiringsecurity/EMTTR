import React, { useEffect } from "react";
import { useLocation } from "react-router-dom";
import HomeContainer from "../containers/HomeContainer";
import TribunalsContextProvider from "../context/TribunalsContext";

function Home() {
  const location = useLocation();

  useEffect(() => {
    if (location.hash) {
      let elem = document.getElementById(location.hash.slice(1));
      if (elem) {
        elem.scrollIntoView({ behavior: "smooth" });
      }
    } else {
      window.scrollTo({ top: 0, left: 0, behavior: "smooth" });
    }
  }, [location]);
  return (
    <TribunalsContextProvider>
      <HomeContainer />
    </TribunalsContextProvider>
  );
}

export default Home;
