export function shortAddress(address) {
  if (address > 6) {
    return `${address.substring(0, 6)}…${address.substring(
      address.length - 4
    )}`;
  } else {
    return address;
  }
}
