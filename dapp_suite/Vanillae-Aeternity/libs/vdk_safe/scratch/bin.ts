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
    let result_bits       : bits       = bits_zeros(result_bit_length);
    let result_bytes      : Uint8Array = result_bits.bytes;

    // alright so
    // we can start by copying the first bytes into result bytes
    for (let bytes1_idx0 = 0;
             bytes1_idx0 < bytes1.length;
             bytes1_idx0++)
    {
       result_bytes[bytes1_idx0] = bytes1[bytes1_idx0];
    }

    // next
    // we need to calculate the left-shift offset
    // this will be 8 - (bytes1.bit_length % 8)
    let num_trailing_zeros_in_first_array : number = 8 - (bits1.bit_length % 8);
    // so
    // bytes1: ABCD_EF00
    // bytes2: GH12_3000
    // result: ABCD_EFGH 1230_0000
    // ah ok, so we need to for each byte in the second array
    // take the first however many bits, xor it with the existing byte
    // then take the last however many bits and place them into the next byte
    // this is super confusing but
    // ABCD_EF00
    //           GH12_3456
    // operation:
    //     ABCD_EF00
    // xor 0000_00GH
    //   = ABCD_EFGH 1234_5600
    //
    // then on the next iteration
    // 1234_5600
    //          abcd_efgh
    // ->
    // 1234_56ab cdef_gh00
    //
    // ah so there's a pattern
    // however many trailing 0s there are in the first array
    // say there's 2
    // we take the first 2 bits of the upcoming byte
    // xor that against the current byte
    // take the last 6 bits of the upcoming byte
    // set the next byte to that
    //
    // have to think about edge behavior
    // this is ripe for off-by-1 errors
    // but i think the general idea is right
    //
    // so we start the iteration
    // on the last byte of the first array
    let last_byte_of_first_array_idx0            : number = bytes1.length - 1;
    // and we end
    // on the second-to-last-byte of the result array
    let second_to_last_byte_of_result_array_idx0 : number = result_bytes.length - 2;
    // the reason we do that is because we're doing this is because we are
    // going along, xoring against the current byte and then setting the next
    // byte
    //
    // ok so
    for (let this_result_byte_idx0  = last_byte_of_first_array_idx0;
             this_result_byte_idx0 <= second_to_last_byte_of_result_array_idx0;
             this_result_byte_idx0++)
    {
       let this_result_byte : number = result_bytes[this_result_byte_idx0];

       // ok here we need to fish out the relevant byte of the second array
       // gaaah
       // so this will be 0 at the start of the loop
       let relevant_byte_of_second_array_idx0 : number = this_result_byte_idx0 - last_byte_of_first_array_idx0;
       let relevant_byte_of_second_array      : number = bytes2[relevant_byte_of_second_array_idx0];

       // ok so let's fish out the leading digits
       // the number of leading digits is the number of trailing 0s in the first array
       let num_leading_digits  : number = num_trailing_zeros_in_first_array;
       let num_trailing_digits : number = 8 - num_leading_digits;

       // suppose there are 2 leading digits and 6 trailing digits
       // ABCD_EFGH
       // leading digits are
       // ABCD_EFGH >> 6 = 0000_00AB
       // trailing digits are
       // (ABCD_EFGH << 2) % 255 = CDEF_GH00
       let leading_digits  : number = relevant_byte_of_second_array >> num_trailing_digits;
       let trailing_digits : number = (relevant_byte_of_second_array << num_leading_digits) % 255;

       // xor the current byte against the leading digits
       let new_this_result_byte : number = this_result_byte ^ leading_digits;
       result_bytes[this_result_byte_idx0] = new_this_result_byte;

       // set the next byte to the trailing digits
       result_bytes[this_result_byte_idx0 + 1] = trailing_digits;
    }

     // i think we're done
     return {bit_length : result_bit_length,
             bytes      : result_bytes};
}

