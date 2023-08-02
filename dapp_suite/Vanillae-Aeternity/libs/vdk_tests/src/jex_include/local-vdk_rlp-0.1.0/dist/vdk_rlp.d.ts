/**
 * Ethereum Recursive-Length Prefix Encoding implementation
 *
 * Two reference implementations:
 *
 * 1. Ethereum Python implementation
 * 2. My Erlang implementation (agrees with Ethereum Python in randomized
 *    tests)
 *
 * This work can be found in ../test/testgen/
 *
 * FIXME: need to have Safe assertions for "i am decoding a list", "i am
 *        decoding a binary", "I am decoding something with no remainder", etc
 *
 * @module
 */
export { decoded_data, decode_result, decode, encode };
/**
 * Data that RLP can encode. Also the result of decoding.
 */
declare type decoded_data = Uint8Array | Array<decoded_data>;
/**
 * Decoding can have a remainder
 */
declare type decode_result = {
    decoded_data: decoded_data;
    remainder: Uint8Array;
};
/**
 * Decode an RLP-encoded bytestring into a `decode_result` (decoded data +
 * whatever wasn't consumed)
 */
declare function decode(bytes: Uint8Array): decode_result;
/**
 * Encode some decoded data
 */
declare function encode(data: decoded_data): Uint8Array;
