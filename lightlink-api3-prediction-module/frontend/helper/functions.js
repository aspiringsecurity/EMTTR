export function addDecimalTwoPlacesFromRight(inputString) {
  const length = inputString.length;
  if (length <= 2) {
    // If the length is less than or equal to 2, simply return the string as it is.
    return inputString;
  } else {
    // Insert the decimal point at the appropriate position and return the modified string.
    const modifiedString =
      inputString.slice(0, length - 2) + "." + inputString.slice(length - 2);
    return modifiedString;
  }
}

export function convertUnixEpochToDateString(epoch) {
  const bigEpoch = epoch.toString();

  const date = new Date(parseInt(bigEpoch) * 1000);
  const day = date.getDate().toString().padStart(2, "0");
  const month = (date.getMonth() + 1).toString().padStart(2, "0");
  const year = date.getFullYear().toString();
  const formattedDate = `${day}/${month}/${year}`;

  return formattedDate;
}

export function convertToDecimal(bigNumber) {
  const strNumber = bigNumber.toString(); // Convert to string
  const decimalIndex = strNumber.length - 18; // Index to place decimal

  // Insert decimal point at the appropriate index
  const result = [
    strNumber.slice(0, decimalIndex),
    ".",
    strNumber.slice(decimalIndex),
  ].join("");

  // Round the result to 5 decimal places
  const final = result.substring(0, decimalIndex + 7);
  return final;
}

export function addDecimalSixPlacesFromRightAndRemoveTrail(inputString) {
  const length = inputString.length;
  if (length <= 6) {
    // If the length is less than or equal to 6, simply return the string as it is.
    return inputString;
  } else {
    // Insert the decimal point at the appropriate position and return the modified string.
    const modifiedString =
      inputString.slice(0, length - 6) + "." + inputString.slice(length - 6);

    return modifiedString.replace(/(\.\d*?[1-9])0+$/, "$1").replace(/\.$/, "");
  }
}
