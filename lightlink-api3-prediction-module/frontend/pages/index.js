import HomeLayout from "./components/layout/HomeLayout";

import { useState, useEffect } from "react";

const FontWrapper = ({ children }) => {
  const [isVisible, setIsVisible] = useState(false);

  useEffect(() => {
    // Wait for the font to load before showing the text
    document.fonts.ready.then(() => setIsVisible(true));
  }, []);

  return (
    <div style={{ visibility: isVisible ? "visible" : "hidden" }}>
      {children}
    </div>
  );
};
export default function Home() {
  return (
    <>
      <FontWrapper>
        <HomeLayout />
      </FontWrapper>
    </>
  );
}
