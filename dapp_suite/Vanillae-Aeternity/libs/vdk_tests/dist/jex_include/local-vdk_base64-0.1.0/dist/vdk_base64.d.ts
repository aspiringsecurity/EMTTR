/**
 * Base64 Utility Functions in TypeScript
 */
export { encode, decode };
/**
 * Encode an array of bytes as a Uint8Array in base64 notation.
 */
declare function encode(bytes: Uint8Array): string;
/**
 * Decode a base64-encoded string
 */
declare function decode(base64_str: string): Uint8Array;
