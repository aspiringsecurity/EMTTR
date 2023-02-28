import React from "react";
import background from "../assets/sea.jpg";
import { Link, useNavigate } from "react-router-dom";
import icon from "../assets/favicon.png";

export function Navigation() {
  // eslint-disable-next-line
  const navigate = useNavigate();

  return (
    <div>
      <nav className="navbar navbar-expand navbar-dark bg-dark">
        <div className="container">
          <a className="navbar-brand col-sm" href="/">
            <b className="center-vertical" style={{ marginLeft: "-3px" }}>
              <img
                className="mr-1"
                src={icon}
                width={32}
                height={32}
                alt="image_logo"
              />{" "}
              Ocean Storm
            </b>
            <h6 display="inline">by InsureBlox</h6>
          </a>

          <div className="collapse navbar-collapse" id="navbarSupportedContent">
            <ul className="navbar-nav mr-auto">
              <li
                className={`nav-item ${
                  window.location.pathname === "/" ? "active" : ""
                }`}
              >
                <Link to="/" className="nav-link">
                  Home
                </Link>
              </li>
              <li
                className={`nav-item ${
                  window.location.pathname === "/contracts" ? "active" : ""
                }`}
              >
                <Link to="/contracts" className="nav-link">
                  Contracts
                </Link>
              </li>
            </ul>
          </div>
        </div>
      </nav>

      <div
        style={{
          padding: "0",
          height: "6em",
          overflow: "hidden",
          marginBottom: "2rem",
          backgroundColor: "#e9ecef"
        }}
      >
        <img
          className="img-fluid"
          src={background}
          alt=""
          style={{ width: "100%" }}
        />
      </div>
    </div>
  );
}
