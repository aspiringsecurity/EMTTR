const truncateLongNames = (name) => {
  if (name.length > 20) return `${name.slice(0, 6)}...${name.slice(-5)}`;
  return name;
};

export default truncateLongNames;
