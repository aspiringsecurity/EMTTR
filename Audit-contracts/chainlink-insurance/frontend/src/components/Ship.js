import React from "react";
import { Link, useNavigate } from "react-router-dom";
import vessels from "../api/vessels.json";
import "./contracts.css";

const getShipViaId = (shipId) => {
  const vesselFiltered = vessels.data.vessels.filter(
    (vessel) => vessel.uuid === shipId
  );
  if (vesselFiltered.length === 1) {
    return vesselFiltered[0];
  } else {
    return null;
  }
};

export function Ship() {
  // eslint-disable-next-line
  const navigate = useNavigate();

  const shipId = window.location.pathname.replace("/ship/", "");
  const ship = getShipViaId(shipId);

  if (!ship)
    return (
      <div>
        {" "}
        <h1>
          <b>Ship not found</b>
        </h1>
        <Link to="/contracts">Back</Link>
      </div>
    );

  return (
    <div>
      <h1>
        <b>Ship</b>
      </h1>
      <div className="pb-3">{shipId}</div>
      <div className="pb-3">
        <Link to="/contracts">Back</Link>
      </div>
      <div className="tableWrapper">
        <div className="tableWrapper2">
          <table>
            <tbody>
              {Object.keys(ship).map((key, index) => {
                return (
                  <tr key={index}>
                    <td>{key}</td>
                    <td>{ship[key]}</td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}
