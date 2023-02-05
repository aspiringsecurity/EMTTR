import React from "react";
import { Link } from "react-router-dom";
import { truncateWithEllipsis } from "../api/utils";
import Tag from "./Tag";

const DaoCard = ({ logo, title, desc, slug, isTest }) => {


  return (
    <Link to={`/${slug}/proposals/`} className="xl:w-1/3 md:w-1/2 w-full p-4">
      <div className="border border-gray-200 hover:border-gold p-6 rounded-lg relative">
        {isTest && <Tag text="Test" bg="bg-cadet" />}
        <img
          alt=""
          src={logo}
          className=" w-10 h-10 inline-flex items-center justify-center rounded-full bg-indigo-100 border-0 mb-4"
        />

        <h2 className="text-lg text-gray-900 font-serif font-medium title-font mb-2">
          {title}
        </h2>
        <p className="leading-relaxed text-gray-500 text-base min-h-[52px] min-h-[52px] xl:min-h-[78px]">
          {truncateWithEllipsis(desc, 130, "end")}
        </p>
      </div>
    </Link>
  );
};

export default DaoCard;
