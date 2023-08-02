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
export { decode, encode };
/**
 * Decode an RLP-encoded bytestring into a `decode_result` (decoded data +
 * whatever wasn't consumed)
 */
function decode(bytes) {
    // check the first byte
    let first_byte = bytes[0];
    let rest = bytes.slice(1);
    // if the first byte is between 0 and 127, that is the data
    if (first_byte <= 127) {
        return dr(new Uint8Array([first_byte]), rest);
    }
    // if the first byte is between 128 and 183 = 128 + 55, it is a bytestring
    // and the length is Byte - 128
    else if (first_byte <= 183) {
        let payload_byte_length = first_byte - 128;
        let payload = rest.slice(0, payload_byte_length);
        let rest2 = rest.slice(payload_byte_length);
        return dr(payload, rest2);
    }
    // if the first byte is between 184 = 183 + 1 and 191 = 183 + 8, it is a
    // bytestring. the byte length of bytestring is FirstByte - 183. Then pull
    // out the actual data
    else if (first_byte <= 191) {
        let byte_length_of_byte_length = first_byte - 183;
        let bytes_of_byte_length = rest.slice(0, byte_length_of_byte_length);
        let byte_length = bytes_to_number(bytes_of_byte_length);
        let bytes = rest.slice(byte_length_of_byte_length, byte_length + byte_length_of_byte_length);
        let rest2 = rest.slice(byte_length + byte_length_of_byte_length);
        return dr(bytes, rest2);
    }
    // If the first byte is between 192 and 247 = 192 + 55, it is a list. The
    // byte length of the list-payload is FirstByte - 192. Then the list
    // payload, which needs to be decoded on its own.
    else if (first_byte <= 247) {
        let byte_length_of_list = first_byte - 192;
        let list_payload = rest.slice(0, byte_length_of_list);
        let list = decode_list(list_payload);
        let rest2 = rest.slice(byte_length_of_list);
        return dr(list, rest2);
    }
    // If the first byte is between 248 = 247 + 1 and 255 = 247 + 8, it is a
    // list.  The byte length of the byte length of the list-payload is
    // FirstByte - 247.  Then the byte length of the list. Then the list
    // payload, which needs to be decoded on its own.
    else {
        let byte_length_of_byte_length = first_byte - 247;
        let bytes_of_byte_length = rest.slice(0, byte_length_of_byte_length);
        let byte_length = bytes_to_number(bytes_of_byte_length);
        let list_bytes = rest.slice(byte_length_of_byte_length, byte_length + byte_length_of_byte_length);
        let list = decode_list(list_bytes);
        let rest2 = rest.slice(byte_length + byte_length_of_byte_length);
        return dr(list, rest2);
    }
}
/**
 * Decode a list payload (non-prefixed) into an `Array<decoded_data>`.
 *
 * Repeatedly decodes an element off the list until remainder is empty.
 *
 * @internal
 */
function decode_list(bytes) {
    let arr = [];
    while (bytes.length > 0) {
        // grab an item off the bytes
        let { decoded_data, remainder } = decode(bytes);
        // push it
        arr.push(decoded_data);
        // update bytes
        bytes = remainder;
    }
    return arr;
}
/**
 * Convert bytestring to number
 *
 * @internal
 */
function bytes_to_number(bytes) {
    let n = 0;
    for (let b of bytes) {
        n <<= 8;
        n += b;
    }
    return n;
}
/**
 * Make a `decode_result` containing the two arguments
 *
 * @internal
 */
function dr(x, y) {
    return { decoded_data: x,
        remainder: y };
}
//=============================================================================
//=============================================================================
// ENCODING
//=============================================================================
//=============================================================================
/**
 * Encode some decoded data
 */
function encode(data) {
    // is it an array or data
    if (is_binary(data)) {
        return encode_binary(data);
    }
    else if (is_list(data)) {
        return encode_list(data);
    }
    else {
        throw new Error('encode told to encode something that is not an array or a binary: ' + data);
    }
}
/**
 * Encode a binary into RLP
 *
 * @internal
 */
function encode_binary(bytes) {
    let len = bytes.length;
    // single byte case when the byte is between 0..127
    // result is the bytestring containing the byte itself
    if ((len === 1) &&
        (bytes[0] <= 127)) {
        return bytes;
    }
    // if the bytestring is 0..55 bytes long, the first byte in the result is
    // 128 + Length, the rest of the result bytestring is the input string
    else if (len <= 55) {
        // construct the result
        // <<128 + Len, Bytes/binary>>
        let result = new Uint8Array(len + 1);
        // first byte is 128 + length
        result[0] = 128 + len;
        // copy input bytes into result
        for (let input_idx0 = 0; input_idx0 < len; input_idx0++) {
            let result_idx0 = input_idx0 + 1;
            result[result_idx0] = bytes[input_idx0];
        }
        return result;
    }
    // if the bytestring is more than 55 bytes long, the first byte is 183 +
    // ByteLengthOfByteLength, followed by the byte length, followed by the
    // bytes
    else {
        let len_bytes = encode_unsigned(len);
        let len_bytes_length = len_bytes.length;
        // total array is
        // <<183 + len_bytes_length,       len_bytes,           Bytes>>
        //      1 byte              len_bytes_length bytes    len bytes
        let result = new Uint8Array(1 + len_bytes_length + len);
        // <<183 + len_bytes_length,       len_bytes,           Bytes>>
        //      1 byte              len_bytes_length bytes    len bytes
        //
        //   ^ YOU ARE HERE
        result[0] = 183 + len_bytes_length;
        // copy len_bytes into result
        for (let len_bytes_idx0 = 0; len_bytes_idx0 < len_bytes_length; len_bytes_idx0++) {
            // <<183 + len_bytes_length,        len_bytes,           Bytes>>
            //      1 byte               len_bytes_length bytes    len bytes
            //
            //                          ^ YOU ARE HERE
            let result_idx0 = len_bytes_idx0 + 1;
            result[result_idx0] = len_bytes[len_bytes_idx0];
        }
        // copy original byte array into result
        for (let input_idx0 = 0; input_idx0 < len; input_idx0++) {
            // <<183 + len_bytes_length,        len_bytes,           Bytes>>
            //      1 byte               len_bytes_length bytes    len bytes
            //
            //                                                   ^ YOU ARE HERE
            let result_idx0 = input_idx0 + (1 + len_bytes_length);
            result[result_idx0] = bytes[input_idx0];
        }
        // finally, return the result
        return result;
    }
}
/**
 * Encode a list
 *
 * @internal
 */
function encode_list(list) {
    // first encode every element in the list, then branch
    let payloads = list.map(encode);
    let payload = uint8arr_concat(payloads);
    let payload_size = payload.length;
    // if the payload size is in 0..55, then the first byte is 192 + payload
    // size, followed by the payload
    if (payload_size <= 55) {
        // result: <<(192 + Payload_Size),     Payload/binary     >>;
        //              1 byte              payload_size bytes
        let result = new Uint8Array(1 + payload_size);
        // result: <<(192 + Payload_Size),     Payload/binary     >>;
        //              1 byte              payload_size bytes
        //
        //           ^ YOU ARE HERE
        result[0] = 192 + payload_size;
        // copy the rest of the payload into result
        for (let payload_idx0 = 0; payload_idx0 < payload_size; payload_idx0++) {
            // result: <<(192 + Payload_Size),     Payload/binary     >>;
            //              1 byte              payload_size bytes
            //
            //                                 ^ YOU ARE HERE
            let result_idx0 = payload_idx0 + 1;
            result[result_idx0] = payload[payload_idx0];
        }
        return result;
    }
    // if the payload size is greater than 55, the first byte is 247 +
    // size_of_payload_size, followed by the payload size, followed by the
    // payload
    else {
        // compute the payload size size
        let payload_size_bytes = encode_unsigned(payload_size);
        let payload_size_size = payload_size_bytes.length;
        // result = <<(247 + payload_size_size),   payload_size_bytes/binary,   payload/binary      >>
        //              1 byte                      payload_size_size bytes    payload_size bytes
        let result = new Uint8Array(1 + payload_size_size + payload_size);
        // result = <<(247 + payload_size_size),   payload_size_bytes/binary,   payload/binary      >>
        //              1 byte                      payload_size_size bytes    payload_size bytes
        //
        //            ^ YOU ARE HERE
        // first byte is 247 + payload_size_size
        result[0] = 247 + payload_size_size;
        // copy the payload_size_bytes into result
        for (let psb_idx0 = 0; psb_idx0 < payload_size_size; psb_idx0++) {
            // result = <<(247 + payload_size_size),   payload_size_bytes/binary,   payload/binary      >>
            //              1 byte                      payload_size_size bytes    payload_size bytes
            //
            //                                       ^ YOU ARE HERE
            // offset is 1 byte for this
            let result_idx0 = psb_idx0 + 1;
            result[result_idx0] = payload_size_bytes[psb_idx0];
        }
        // copy the payload into result
        for (let pb_idx0 = 0; pb_idx0 < payload_size; pb_idx0++) {
            // result = <<(247 + payload_size_size),   payload_size_bytes/binary,   payload/binary      >>
            //              1 byte                      payload_size_size bytes    payload_size bytes
            //
            //                                                                   ^ YOU ARE HERE
            // offset is 1 + payload_size_size bytes for this
            let result_idx0 = pb_idx0 + (1 + payload_size_size);
            result[result_idx0] = payload[pb_idx0];
        }
        // finally, return result
        return result;
    }
}
/**
 * Encode a number as a bytestring
 *
 * Not for general use, this assumes the input number is greater than 55
 *
 * @internal
 */
function encode_unsigned(n) {
    // can assume that n initially is greater than 55
    // have to encode it in reverse order
    let arr = [];
    // repeated division by 256 with remainder
    //
    // example: suppose bign was 1234 and we were dividing by 10
    //
    // iteration 0:
    //      bign      = 1234
    //      arr       = []
    //      ---
    //      bign / 10 = 123
    //      bign % 10 = 4
    //      ---
    //      bign      = 123
    //      arr       = [4]
    //
    // iteration 1:
    //      bign      = 123
    //      arr       = [4]
    //      ---
    //      bign / 10 = 12
    //      bign % 10 = 3
    //      ---
    //      bign      = 12
    //      arr       = [4, 3]
    //
    // iteration 2:
    //      bign      = 12
    //      arr       = [4, 3]
    //      ---
    //      bign / 10 = 1
    //      bign % 10 = 2
    //      ---
    //      bign      = 1
    //      arr       = [4, 3, 2]
    //
    // iteration 3:
    //      bign      = 1
    //      arr       = [4, 3, 2]
    //      ---
    //      bign / 10 = 0
    //      bign % 10 = 1
    //      ---
    //      bign      = 0
    //      arr       = [4, 3, 2, 1]
    //
    // iteration 4:
    //      bign      = 0
    //      arr       = [4, 3, 2, 1]
    //      ---
    //      DONE
    while (n > 0) {
        arr.push(n % 256);
        n >>= 8;
    }
    // reverse and make into Uint8Array
    arr.reverse();
    return new Uint8Array(arr);
}
/**
 * concatenate an array of `Uint8Array`s into a single Uint8Array
 *
 * @internal
 */
function uint8arr_concat(arrs) {
    // total length
    let total_len = arrs.reduce(// fold
    function (acc_len, this_uint8array) {
        return acc_len + this_uint8array.length;
    }, 
    // initial accumulator
    0);
    // start up result
    let result = new Uint8Array(total_len);
    let result_idx0 = 0;
    // concatenate
    for (let this_uint8arr of arrs) {
        // bytewise copy of this uint8array
        for (let this_uint8arr_idx0 = 0; this_uint8arr_idx0 < this_uint8arr.length; this_uint8arr_idx0++) {
            // copy byte and increment result idx0
            result[result_idx0] = this_uint8arr[this_uint8arr_idx0];
            result_idx0++;
        }
    }
    return result;
}
/**
 * returns true if input is `instanceof Uint8Array`
 *
 * @internal
 */
function is_binary(x) {
    return (x instanceof Uint8Array);
}
/**
 * returns true if input is `instanceof Array`
 *
 * @internal
 */
function is_list(x) {
    return (x instanceof Array);
}
//# sourceMappingURL=vdk_rlp.js.map