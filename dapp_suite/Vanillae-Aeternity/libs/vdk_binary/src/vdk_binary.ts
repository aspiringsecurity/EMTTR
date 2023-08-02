/**
 * Miscellaneous binary utility functions
 *
 * @module
 */

export {
    // basic bytes functions
    bytes_eq,
    bytes_concat,
    strong_rand_bytes,
    bytes_to_bigint,
    bigint_to_bytes,
    // basic bits functions
    bits_null,
    bits_zeros,
    bits_ones,
    bits_i0th,
    bits_concat
};

export type {
    bits
};



/**
 * Bytewise equality of two `Uint8Array`s
 */
function
bytes_eq
    (arr1 : Uint8Array,
     arr2 : Uint8Array)
    : boolean
{
    if (arr1.length !== arr2.length)
        return false;
    else
    {
        let len : number = arr1.length;
        for (let i = 0;
                 i < len;
                 i++)
        {
            if (arr1[i] !== arr2[i])
                return false;
        }

        return true;
    }
}



/**
 * Concatenate two arrays
 */
function
bytes_concat
    (arr1 : Uint8Array,
     arr2 : Uint8Array)
    : Uint8Array
{
    let len1             : number     = arr1.length;
    let len2             : number     = arr2.length;
    let arr1_idx0_offset : number     = 0;
    let arr2_idx0_offset : number     = len1;
    let result_len       : number     = len1 + len2;
    let result           : Uint8Array = new Uint8Array(result_len);
    // copy first array into result
    for (let arr1_idx0 = 0;
             arr1_idx0 < len1;
             arr1_idx0++)
    {
        // no offset here
        let result_idx0 : number = arr1_idx0 + arr1_idx0_offset;
        result[result_idx0] = arr1[arr1_idx0];
    }
    // copy second array into result
    for (let arr2_idx0 = 0;
             arr2_idx0 < len2;
             arr2_idx0++)
    {
        // offset by the length of the first array
        let result_idx0 : number = arr2_idx0 + arr2_idx0_offset;
        result[result_idx0] = arr2[arr2_idx0];
    }
    return result;
}



/**
 * Cryptographically random bytes
 */
function
strong_rand_bytes
    (how_many : number)
    : Uint8Array
{
    let arr = new Uint8Array(how_many);
    (new Crypto()).getRandomValues(arr);
    return arr;
}



/**
 * Convert a byte array to a bigint
 *
 * Equivalent to `binary:decode_unsigned/1` from Erlang
 */
function
bytes_to_bigint
    (bytes: Uint8Array)
    : bigint
{
    let n : bigint = 0n;
    for (let b of bytes) {
        // move first, then add
        // otherwise it ends on a move
        // imperative languages are for losers
        n <<= 8n;
        n  += BigInt(b);
    }
    return n;
}



/**
 * Convert a bigint to a byte array
 *
 * Equivalent to `binary:encode_unsigned/1` from Erlang
 *
 * Requires input to be positive
 */
function
bigint_to_bytes
    (q: bigint)
    : Uint8Array
{
    if (q < 0n) {
        throw new Error('q < 0n: ' + q);
    }

    let arr_reverse = [];
    while (q > 0n) {
        let r = Number(q % 256n);
        q /= 256n;
        arr_reverse.push(r);
    }
    arr_reverse.reverse();
    return new Uint8Array(arr_reverse);
}



/**
 * Oh no, bitstrings in a language that only has bytestrings
 *
 * By convention these are `Uint8Array`s with byte length `ceil(bit_length /
 * 8)`, and all trailing bits are zero.
 */
type bits =
    {bit_length : number,
     bytes      : Uint8Array};



/**
 * Get an uninitialized bitstring
 *
 * @internal
 */
function
bits_null
    (bit_length : number)
    : bits
{
    let byte_length : number     = Math.ceil(bit_length / 8);
    let result      : Uint8Array = new Uint8Array(byte_length);
    return {bit_length : bit_length,
            bytes      : result};
}



/**
 * Get a bitstring of a given length where every value is 0.
 */
function
bits_zeros
    (bit_length : number)
    : bits
{
    let byte_length : number     = Math.ceil(bit_length / 8);
    let result      : Uint8Array = new Uint8Array(byte_length);
    for (let i0 = 0;
             i0 < byte_length;
             i0++)
    {
        result[i0] = 0;
    }
    return {bit_length : bit_length,
            bytes      : result};
}



/**
 * Get a bitstring of a given length where every value is 1.
 */
function
bits_ones
    (bit_length : number)
    : bits
{
    let byte_length : number     = Math.ceil(bit_length / 8);
    let result      : Uint8Array = new Uint8Array(byte_length);

    // fill everything except the last byte with 255s
    for (let i0 = 0;
             i0 < (byte_length - 1);
             i0++)
    {
        result[i0] = 255;
    }

    // alright so the last byte
    // ok so the number of leading 0s is
    // 8 - (bit_length % 8)
    let num_trailing_zero_bits : number =  8 - (bit_length % 8);
    // the trailing byte is 255 << that
    // e.g. 3 trailing 0s
    // 1111_1111 -> 1111_1000
    let last_byte      : number = 255 << num_trailing_zero_bits;
    let last_byte_idx0 : number = byte_length - 1;
    result[last_byte_idx0] = last_byte;

    return {bit_length : bit_length,
            bytes      : result};
}



/**
 * Get the bit at a given 0-index
 */
function
bits_i0th
    (bit_idx0  : number,
     bits      : bits)
    : number
{
    // first task is figuring out what byte we're at
    // for instance if we want bit 27
    // 3*8 = 24 =< 27 < 4*8
    // so it's Math.floor(bit_idx0 / 8)
    let byte_idx0 : number = Math.floor(bit_idx0 / 8);
    // let's fetch the byte and work with that
    let the_byte  : number = bits.bytes[byte_idx0];

    // ok so let's go with 27 again
    // 27 = 3 mod 8
    // so we bitshift right by (8 - 3)
    // and then take the remainder dividing by 2
    // --B-_---- -> ----_---B -> 0000_000B
    let bsr : number = 8 - (bit_idx0 % 8);
    return (the_byte >> bsr) % 2;
}



/**
 * Concatenate two bitstrings
 */
function
bits_concat
    (bits1 : bits,
     bits2 : bits)
    : bits
{
    let result_bit_length : number     = bits1.bit_length + bits2.bit_length;
    let bytes1            : Uint8Array = bits1.bytes;
    let bytes2            : Uint8Array = bits2.bytes;
    // using zeros here because of our xor trick in a minute
    let result_bits       : bits       = bits_null(result_bit_length);
    let result_bytes      : Uint8Array = result_bits.bytes;

    // go along each byte in result, and compute the byte boundary
    for (let i = 0;
             i < result_bytes.length;
             i++)
    {
        // ABCD_EFGH _
        // 0123_4567 8
        // this is the bit index of the leftmost bit in this byte
        let start_bit_bi0               : number = i * 8;
        let next_start_bit_bi0          : number = start_bit_bi0 + 8;
        // does the bit at the beginning of this byte correspond to the first array?
        // strict comparison:
        //      suppose i = 0,
        //      suppose bit_length1 is 0
        //          then this says no, go to second array
        //      suppose bl1 = 1,
        //          this says start at first array
        let start_bit_is_of_first_array : boolean = start_bit_bi0 < bits1.bit_length;
        // weak comparison:
        //      suppose bit_length1 = 8
        //          ABCD_EFGH        _
        //          0123_4567        8
        //          ^
        //          start_bit_bi0    ^ next_start_bit_bi0
        let stop_bit_is_of_first_array  : boolean = next_start_bit_bi0 <= bits1.bit_length;
        // is this a bytes1 byte
        let is_bytes1_byte   : boolean = start_bit_is_of_first_array && stop_bit_is_of_first_array;
        let is_boundary_byte : boolean = start_bit_is_of_first_array && !stop_bit_is_of_first_array;

        // need to work out the ping_pong bs up here because js is dumb and I
        // can't put lets between elseifs
        // alright, now we're in the case of only copying from the second array
        // we have two cases:
        //      ping-pong:
        //          ABCD_EFGH 1234_5678
        //             - ---- ---
        //          copying these bits
        //      ping:
        //          ABCD_EFGH <end>
        //             - ---- 000
        //
        // how to distinguish between these two??
        //
        // we're in the ping case when the start_bit_bi0 corresponds to the
        // final byte of the second array
        //
        // ok
        //
        // we need to compute the bit address in the second array that
        // corresponds to the bit address at the beginning of this byte in the
        // result array
        let bits2_addr_bi0 : number = start_bit_bi0 - bits1.bit_length;
        // I think the variable is
        //      bits_left_to_copy = bits2.bit_length - bit_addr2_bi0
        //      no + 1 because the current bit is uncopied,
        //      so if bit_addr2_bi0 = 7 and bits2.bit_length is 8, it means we
        //      have the last bit to copy
        // cases:
        //      bits_left_to_copy <= 0 ->
        //          this would mean we have more bits to copy, but are out of
        //          source bits. should be impossible if bits_null is correct
        //      bits_left_to_copy <= 8 ->
        //          this would mean we are going to fill this last byte in the
        //          result array with the correct bits from array2, but how we
        //          do this will depend on how those are arranged in bytes2
        //          (whether we grab one or two bytes)
        //
        //          this is the tricky case
        //
        //          I think what matters here is the byte address in the second array
        //          if we're on the last byte
        //      8 < bits_left_to_copy ->
        //          this means we can safely grab two bytes from bytes2, and do
        //          our bitshifting to make it correct
        //
        // FIXME
        let bits_left_to_copy : number = bits2.bit_length - bits2_addr_bi0;
        // no the variable that matters is which byte we're on in the result
        let this_bytes2_addr_i0 : number = Math.floor(bits2_addr_bi0 / 8);
        let next_bytes2_addr_i0 : number = this_bytes2_addr_i0 + 1;
        let bitshift_amt        : number = bits1.bit_length % 8;

        let ping : boolean = next_bytes2_addr_i0 === bytes2.length;


        // simple case: this is a byte from the first array
        // just copy it
        if (is_bytes1_byte)
            result_bytes[i] = bytes1[i];
        // this is a boundary byte
        // further cases:
        //      second bit length is 0 ->
        //          ABCD_EF-- <empty>
        //          ^ starting here
        //          just copy first byte and move along
        else if (is_boundary_byte  && (bits2.bit_length === 0))
            result_bytes[i] = bytes1[i];
        // boundary byte, and there is at least one byte in the second array
        else if (is_boundary_byte)
        {
            // copy over the first byte
            result_bytes[i] = bytes1[i];
            // take the first byte from the second array
            let first_byte_of_second_array : number = bytes2[0];
            // bytes1:
            //      ABCD_EF00
            //      6
            // bytes2:
            //      1234_5678
            //      0000_0012
            // and bitshift it right by that amount
            let bitshift_amt : number = bits1.bit_length % 8;
            result_bytes[i] ^= first_byte_of_second_array >> bitshift_amt;
        }
        // last byte of second array
        else if (ping)
            result_bytes[i] = (bytes2[this_bytes2_addr_i0] << bitshift_amt);
        // ping-pong: not last byte of second array
        else
            result_bytes[i] = (bytes2[this_bytes2_addr_i0] << bitshift_amt) ^ (bytes2[next_bytes2_addr_i0] << bitshift_amt);
    }

    return {bit_length : result_bit_length,
            bytes      : result_bytes};

}
