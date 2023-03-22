export const parseAddressToNumber = (address: string): number => parseInt(address.slice(2, 10), 16)
