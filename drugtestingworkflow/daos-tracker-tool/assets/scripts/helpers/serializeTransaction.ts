type SignatureKey = string | SignatureKey[]

export const serializeTransaction = (array: SignatureKey[]): string => array
  .filter((item) => !Array.isArray(item) || item.length)
  .map((item) => (Array.isArray(item) ? `{${serializeTransaction(item)}}` : item))
  .join('.')
