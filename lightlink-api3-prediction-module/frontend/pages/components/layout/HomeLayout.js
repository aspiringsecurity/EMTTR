import HomeHeader from "../sections/home/HomeHeader.js";
import HomeHero from "../sections/home/HomeHero.js";
import { useState } from "react";

export default function HomeLayout() {
  const [page, setPage] = useState("Home");

  return (
    <>
      <HomeHeader setPage={setPage} />
      <HomeHero page={page} />
    </>
  );
}
