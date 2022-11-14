import { Fragment } from "react";

const Token = (props) => {
  return (
    <Fragment>
      <tr className="single-channel py-2">
        <td>{props.id + 1}</td>
        <td>
          <img src={props.logo} className="w-8 h-8 rounded-full" alt="" />{" "}
          {props.name}
        </td>
        <td>{props.quote_rate}</td>
        <td>{props.total_liquidity}</td>
        <td>{props.swap_count}</td>
        <td>{props.dex_name}</td>
      </tr>
    </Fragment>
  );
};

export default Token;
