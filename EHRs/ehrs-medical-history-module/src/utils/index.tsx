export const rem = (px: number | string) => {
  return typeof px === 'number'
    ? `${px / 16}rem`
    : `${parseInt(px.split('px')[0]) / 16}rem`
}

export function uint8ArrayToString(uint8Array: Uint8Array) {
  const decoder = new TextDecoder('utf-8');
  return decoder.decode(uint8Array);
}

export function stringToUint8Array(str: string) {
  const encoder = new TextEncoder();
  return encoder.encode(str);
}

export * from './constants'
export * from './interfaces'
