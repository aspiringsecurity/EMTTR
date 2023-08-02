/**
 * Base58 encoding/decoding
 */
export { encode, decode };
/**
 * Encode a Uint8Array into base58
 */
declare function encode(binary: Uint8Array): string;
/**
 * Decode a Base58 string into a Uint8Array
 */
declare function decode(base58: string): Uint8Array;
