import React from "react";
import { Link, useNavigate } from "react-router-dom";

const SideBar = ({ dao, type }) => {
  const navigate = useNavigate();

  const handleJoin = (slug) => {
    navigate(`/mint/${slug}`);
  };

  const SideBarLink = ({ name, url, active }) => (
    <Link
      to={url}
      className={`block px-5 py-2 my-1 sidenav-item hover:bg-gold100 ${
        active ? "border-l-4 border-gold" : ""
      }`}
    >
      {name}
    </Link>
  );
  return (
    <div className="w-1/4 float-left hidden lg:block" id="sidebar-left">
      <div style={{ position: "fixed", width: "240px" }}>
        <div className="md:rounded-xl md:border bg-skin-block-bg border-skin-border text-base overflow-hidden">
          <div className="leading-5 sm:leading-6">
            <div className="text-center h-[253px]">
              <span className="inline-block align-middle leading-none mt-3 mb-2">
                <span className="flex shrink-0 items-center justify-center">
                  <img
                    src={dao.logo}
                    className="rounded-full bg-[color:var(--border-color)]"
                    alt={dao.title}
                    style={{
                      width: "80px",
                      height: "80px",
                      minWidth: "80px",
                    }}
                  />
                </span>
              </span>
              <h3 className="mb-[2px] mx-3 flex justify-center items-center">
                <div className="truncate mr-1 font-serif text-lg mt-3">
                  {dao.title}
                </div>
                <i
                  className="iconfont iconcheck"
                  style={{ fontSize: "20px", lineHeight: "20px" }}
                ></i>
              </h3>
              <div className="mb-[12px] text-skin-text">12K members</div>
              <button
                type="button"
                onClick={() => handleJoin(dao.slug)}
                className={`${
                  dao.creator
                    ? "hover:bg-yellow-600 cursor-pointer"
                    : " opacity-30"
                } text-cadet bg-gold border-0 py-1 px-0 focus:outline-none rounded mb-4 group w-[120px]`}
                disabled={!dao.creator}
              >
                Join
              </button>
            </div>
            <div className="py-2">
              <SideBarLink
                name="Proposals"
                active={type === "proposals"}
                url={`/${dao.slug}/proposals`}
              />
              <SideBarLink
                name="New proposal"
                active={type === "new"}
                url={`/${dao.slug}/proposals/new`}
              />
              <SideBarLink
                name="About"
                active={type === "about"}
                url={`/${dao.slug}/about`}
              />
            </div>
            <div className="text-center">
              <div className="my-3 mx-2 flex justify-center items-center space-x-3">
                <a
                  href="https://twitter.com/ensdomains"
                  target="_blank"
                  className="whitespace-nowrap flex"
                  rel="noopener noreferrer"
                >
                  <i
                    className="iconfont icontwitter hover:opacity-80 text-skin-text"
                    style={{ fontSize: "24px", lineHeight: "24px" }}
                  ></i>
                </a>
                <a
                  href="https://github.com/ensdomains"
                  target="_blank"
                  className="whitespace-nowrap flex"
                  rel="noopener noreferrer"
                >
                  <i
                    className="iconfont icongithub hover:opacity-80 text-skin-text"
                    style={{ fontSize: "24px", lineHeight: "24px" }}
                  ></i>
                </a>
                <a
                  href="about:blank"
                  target="_blank"
                  className="whitespace-nowrap pointer-events-none flex"
                  rel="noopener noreferrer"
                >
                  <i
                    className="iconfont iconearth opacity-10"
                    style={{ fontSize: "24px", lineHeight: "24px" }}
                  ></i>
                </a>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default SideBar;
