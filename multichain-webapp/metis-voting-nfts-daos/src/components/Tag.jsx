import React from 'react'

const Tag = ({text, bg, noAbsolute}) => {
  return (
    <p
      className={`text-xs text-white right-7 pt-0.5 pb-[3px] px-2 rounded-lg ${bg} ${
        noAbsolute ? "mr-2" : "absolute"
      }`}
    >
      {text}
    </p>
  );
}

export default Tag