import React from "react";
import "./date.css";

const DateInput = ({ title, handleChange }) => {
  return (
    <div className="flex items-center justify-center">
      <div className="datepicker relative form-floating mb-3">
        <input
          onChange={handleChange}
          type="date"
          onKeyDown={() => false}
          className="form-control cursor-pointer block w-full px-3 py-1.5 text-base font-normal text-gray-700 bg-white bg-clip-padding border border-solid border-gray-300 rounded transition ease-in-out m-0 focus:text-gray-700 focus:bg-white focus:border-blue-600 focus:outline-none"
          placeholder="Select a date"
        />
        <label
          htmlFor="floatingInput"
          className="text-gray-700 text-[13px] ml-1"
        >
          {title}
        </label>
      </div>
    </div>
  );
};

export default DateInput;
