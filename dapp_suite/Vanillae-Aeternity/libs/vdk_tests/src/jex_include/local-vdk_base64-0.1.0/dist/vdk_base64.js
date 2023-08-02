/**
 * Base64 Utility Functions in TypeScript
 */
export { encode, decode };
/**
 * Encode an array of bytes as a Uint8Array in base64 notation.
 */
function encode(bytes) {
    // slice the array
    // length of head is a multiple of 3
    // treat the tail as a special case
    let { head, tail, tail_len } = slice3k(bytes);
    let head_str = encode_head(head);
    let tail_str = encode_tail(tail, tail_len);
    return head_str + tail_str;
}
/**
 * Take a Uint8Array, take the first 3k (k >= 0) bytes, put them in head, and
 * the remaining 0,1, or 2 bytes, put them in tail
 *
 * @internal
 */
function slice3k(bytes) {
    let len = bytes.length;
    // too lazy to look up how to do integer division in js so this will do
    let tail_len = len % 3;
    let head_len = len - tail_len;
    // for slice:
    // first argument is the 0-index of the start
    // second - first is the length of the slice
    let head = bytes.slice(0, head_len);
    // empty second argument means go to the end
    let tail = bytes.slice(head_len);
    return { head: head,
        tail: tail,
        tail_len: tail_len };
}
/**
 * Encode a Uint8Array whose length is known to be a multiple of 3
 *
 * @internal
 */
function encode_head(head_bytes) {
    // can assume length of bytes is a multiple of 3
    // start index at 0
    // increment by 3
    let head_bytes_len = head_bytes.length;
    let max_idx0 = head_bytes_len - 1;
    let head_str_acc = '';
    for (let this_3slice_start_idx0 = 0; this_3slice_start_idx0 <= max_idx0; this_3slice_start_idx0 += 3) {
        let this_3slice_bytes = head_bytes.slice(this_3slice_start_idx0, this_3slice_start_idx0 + 3);
        let this_3slice_str = encode3(this_3slice_bytes);
        head_str_acc += this_3slice_str;
    }
    return head_str_acc;
}
/**
 * Encode a 3 bytes into base64 notation
 *
 * @internal
 */
function encode3(bytes) {
    let b0 = bytes[0];
    let b1 = bytes[1];
    let b2 = bytes[2];
    // ABCDEFGH 12345678 abcdefgh
    //    b0       b1       b2
    // ABCDEF GH1234 5678ab cdefgh
    //   n0     n1     n2     n3
    let n0 = b0 >> 2;
    // b0            = ABCDEFGH
    // 4             = _____1__
    // b0 % 4        = ______GH
    // (b0 % 4) << 4 = __GH____
    // b1            = 12345678
    // b1 >> 4       = ____1234
    // n1            = __GH1234
    let n1 = ((b0 % 4) << 4) + (b1 >> 4);
    // b1             = 12345678
    // 16             = ___1____
    // b1 % 16        = ____5678
    // (b1 % 16) << 2 = __5678__
    // b2             = abcdefgh
    // b2 >> 6        = ______ab
    // n2             = __5678ab
    let n2 = ((b1 % 16) << 2) + (b2 >> 6);
    // b2 = abcdefgh
    // 64 = _1______
    // n3 = __cdefgh
    let n3 = b2 % 64;
    // convert to chars
    let s0 = int2char(n0);
    let s1 = int2char(n1);
    let s2 = int2char(n2);
    let s3 = int2char(n3);
    // retrvn
    return s0 + s1 + s2 + s3;
}
/**
 * Encode the final 0, 1, or 2 bytes
 *
 * @internal
 */
function encode_tail(tail_bytes, tail_len) {
    switch (tail_len) {
        case 0: return '';
        case 1: return encode1(tail_bytes);
        case 2: return encode2(tail_bytes);
        default:
            throw new Error('encode_tail with tail_len = ' + tail_len);
    }
}
/**
 * Encode a single byte
 *
 * @internal
 */
function encode1(bytes) {
    let b0 = bytes[0];
    // n0            = __ABCDEF
    // b0            = ABCDEFGH
    // b0 >> 2       = __ABCDEF
    let n0 = b0 >> 2;
    // n1            = __GH____
    // b0            = ABCDEFGH
    //  4            = _____1__
    // b0 % 4        = ______GH
    // (b0 % 4) << 4 = __GH____
    let n1 = (b0 % 4) << 4;
    return int2char(n0) + int2char(n1) + '==';
}
/**
 * Encode two bytes
 *
 * @internal
 */
function encode2(bytes) {
    let b0 = bytes[0];
    let b1 = bytes[1];
    // ABCDEFGH 12345678
    //    b0       b1
    // ABCDEF GH1234 5678__
    //   n0     n1     n2
    let n0 = b0 >> 2;
    // b0            = ABCDEFGH
    // 4             = _____1__
    // b0 % 4        = ______GH
    // (b0 % 4) << 4 = __GH____
    // b1            = 12345678
    // b1 >> 4       = ____1234
    // n1            = __GH1234
    let n1 = ((b0 % 4) << 4) + (b1 >> 4);
    // b1             = 12345678
    // 16             = ___1____
    // b1 % 16        = ____5678
    // (b1 % 16) << 2 = __5678__
    // n2             = __5678__
    let n2 = (b1 % 16) << 2;
    // convert to chars
    let s0 = int2char(n0);
    let s1 = int2char(n1);
    let s2 = int2char(n2);
    // retrvn
    return s0 + s1 + s2 + '=';
}
/**
 * Decode a base64-encoded string
 */
function decode(base64_str) {
    // length of the string is guaranteed to be a multiple of 4
    // if the string is empty, return the empty array
    let len = base64_str.length;
    // this branching contains the implicit assertion that the length is a
    // multiple of 4. If this is not true, the bottom branch is triggered.
    // general case goes first because speeeeeed
    if ((4 < len)
        && (0 === (len % 4))) {
        // split the head and tail
        let tail_start_idx0 = len - 4;
        let head_s = base64_str.slice(0, tail_start_idx0);
        let tail_s = base64_str.slice(tail_start_idx0);
        // Using arrays because Uint8Arrays don't have a concat operation
        let head_arr = decode_head(head_s);
        let tail_arr = decode_tail(tail_s);
        // silly to put these in variables but this is exactly the type of
        // situation where JS type insanity shows up
        //
        // see: i forgot
        // > [1,2,3] + [4,5,6]
        // '1,2,34,5,6'
        //
        // Originally, I used + like some sort of moron who codes in a sane
        // language
        //
        // seriously what is this language
        //
        // this is some clown behavior
        let total_arr = head_arr.concat(tail_arr);
        return new Uint8Array(total_arr);
    }
    // special case if the length is exactly 4
    else if (4 === len) {
        // it's just a tail
        return new Uint8Array(decode_tail(base64_str));
    }
    // empty string
    else if (0 === len) {
        return new Uint8Array([]);
    }
    else {
        throw new Error('base64 decode: invalid string length: ' + len);
    }
}
/**
 * Decode a string known to not have any padding
 *
 * @internal
 */
function decode_head(s) {
    // go 4 characters at a time
    let max_i0 = s.length - 1;
    let decoded_acc = [];
    for (let i0 = 0; i0 <= max_i0; i0 += 4) {
        let this_slice_s = s.slice(i0, i0 + 4);
        let this_slice_arr = decode3(this_slice_s);
        // update accumulator
        decoded_acc = decoded_acc.concat(this_slice_arr);
    }
    return decoded_acc;
}
/**
 * Decode 4 characters that correspond to either 3 bytes, 2, bytes, or 1 byte
 *
 * @internal
 */
function decode_tail(s) {
    // all that matters right now is the last 2 chars
    // s0, s1, s2, s3
    // 0 based indexing is so annoying
    let s2 = s[2];
    let s3 = s[3];
    // braaaaaaaaaaaaaaaaaench
    // two equals signs means 1 byte
    if (('=' === s3) && ('=' === s2)) {
        return decode1(s);
    }
    // one equals sign means 2 bytes
    else if (('=' === s3)) {
        return decode2(s);
    }
    // 0 equals signs means 3 bytes
    else {
        return decode3(s);
    }
}
/**
 * Decode a 4-character long base64 string corresponding to 3 bytes
 *
 * @internal
 */
function decode3(s) {
    // pull out strings
    let s0 = s[0];
    let s1 = s[1];
    let s2 = s[2];
    let s3 = s[3];
    // convert to numbers
    let n0 = char2int(s0);
    let n1 = char2int(s1);
    let n2 = char2int(s2);
    let n3 = char2int(s3);
    // abcdef gh1234 5678ab cdefgh
    //   n0     n1    n2      n3
    // abcdefgh 12345678 abcdefgh
    //   b0        b1       b2
    // n0      = __abcdef
    // n1      = __gh1234
    // n0 << 2 = abcdef__
    // n1 >> 4 = ______gh
    // b0      = abcdefgh
    let b0 = (n0 << 2) + (n1 >> 4);
    // n1             = __gh1234
    // 16             = ___1____
    // n1 % 16        = ____1234
    // (n1 % 16) << 4 = 1234____
    // n2             = __5678ab
    // n2 >> 2        = ____5678
    // b1             = 12345678
    let b1 = ((n1 % 16) << 4) + (n2 >> 2);
    // n2             = __5678ab
    // 4              = _____1__
    // n2 % 4         = ______ab
    // (n2 % 4) << 6  = ab______
    // n3             = __cdefgh
    let b2 = ((n2 % 4) << 6) + n3;
    return [b0, b1, b2];
}
/**
 * Decode a 4-character long base64 string corresponding to 2 bytes
 *
 * @internal
 */
function decode2(s) {
    // xyz=
    // pull out strings
    let s0 = s[0];
    let s1 = s[1];
    let s2 = s[2];
    // convert to numbers
    let n0 = char2int(s0);
    let n1 = char2int(s1);
    let n2 = char2int(s2);
    // abcdef gh1234 5678__
    //   n0     n1    n2
    // abcdefgh 12345678
    //   b0        b1
    // n0      = __abcdef
    // n1      = __gh1234
    // n0 << 2 = abcdef__
    // n1 >> 4 = ______gh
    // b0      = abcdefgh
    let b0 = (n0 << 2) + (n1 >> 4);
    // n1             = __gh1234
    // 16             = ___1____
    // n1 % 16        = ____1234
    // (n1 % 16) << 4 = 1234____
    // n2             = __5678__
    // n2 >> 2        = ____5678
    // b1             = 12345678
    let b1 = ((n1 % 16) << 4) + (n2 >> 2);
    return [b0, b1];
}
/**
 * Decode a 4-character long base64 string corresponding to 2 bytes
 *
 * @internal
 */
function decode1(s) {
    // xy==
    // pull out strings
    let s0 = s[0];
    let s1 = s[1];
    // convert to numbers
    let n0 = char2int(s0);
    let n1 = char2int(s1);
    // abcdef gh____
    //   n0     n1
    // abcdefgh
    //   b0
    // n0      = __abcdef
    // n1      = __gh____
    // n0 << 2 = abcdef__
    // n1 >> 4 = ______gh
    // b0      = abcdefgh
    let b0 = (n0 << 2) + (n1 >> 4);
    return [b0];
}
// FIXME: these tables would *probably* be faster if they were made into objects
/**
 * Conversion table for base64 encode
 *
 * @internal
 */
function int2char(n) {
    switch (n) {
        case 0: return 'A';
        case 1: return 'B';
        case 2: return 'C';
        case 3: return 'D';
        case 4: return 'E';
        case 5: return 'F';
        case 6: return 'G';
        case 7: return 'H';
        case 8: return 'I';
        case 9: return 'J';
        case 10: return 'K';
        case 11: return 'L';
        case 12: return 'M';
        case 13: return 'N';
        case 14: return 'O';
        case 15: return 'P';
        case 16: return 'Q';
        case 17: return 'R';
        case 18: return 'S';
        case 19: return 'T';
        case 20: return 'U';
        case 21: return 'V';
        case 22: return 'W';
        case 23: return 'X';
        case 24: return 'Y';
        case 25: return 'Z';
        case 26: return 'a';
        case 27: return 'b';
        case 28: return 'c';
        case 29: return 'd';
        case 30: return 'e';
        case 31: return 'f';
        case 32: return 'g';
        case 33: return 'h';
        case 34: return 'i';
        case 35: return 'j';
        case 36: return 'k';
        case 37: return 'l';
        case 38: return 'm';
        case 39: return 'n';
        case 40: return 'o';
        case 41: return 'p';
        case 42: return 'q';
        case 43: return 'r';
        case 44: return 's';
        case 45: return 't';
        case 46: return 'u';
        case 47: return 'v';
        case 48: return 'w';
        case 49: return 'x';
        case 50: return 'y';
        case 51: return 'z';
        case 52: return '0';
        case 53: return '1';
        case 54: return '2';
        case 55: return '3';
        case 56: return '4';
        case 57: return '5';
        case 58: return '6';
        case 59: return '7';
        case 60: return '8';
        case 61: return '9';
        case 62: return '+';
        case 63: return '/';
        default: throw new Error("invalid base64 encode byte: " + n);
    }
}
/**
 * Conversion table for base64 decode
 *
 * @internal
 */
function char2int(s) {
    switch (s) {
        case 'A': return 0;
        case 'B': return 1;
        case 'C': return 2;
        case 'D': return 3;
        case 'E': return 4;
        case 'F': return 5;
        case 'G': return 6;
        case 'H': return 7;
        case 'I': return 8;
        case 'J': return 9;
        case 'K': return 10;
        case 'L': return 11;
        case 'M': return 12;
        case 'N': return 13;
        case 'O': return 14;
        case 'P': return 15;
        case 'Q': return 16;
        case 'R': return 17;
        case 'S': return 18;
        case 'T': return 19;
        case 'U': return 20;
        case 'V': return 21;
        case 'W': return 22;
        case 'X': return 23;
        case 'Y': return 24;
        case 'Z': return 25;
        case 'a': return 26;
        case 'b': return 27;
        case 'c': return 28;
        case 'd': return 29;
        case 'e': return 30;
        case 'f': return 31;
        case 'g': return 32;
        case 'h': return 33;
        case 'i': return 34;
        case 'j': return 35;
        case 'k': return 36;
        case 'l': return 37;
        case 'm': return 38;
        case 'n': return 39;
        case 'o': return 40;
        case 'p': return 41;
        case 'q': return 42;
        case 'r': return 43;
        case 's': return 44;
        case 't': return 45;
        case 'u': return 46;
        case 'v': return 47;
        case 'w': return 48;
        case 'x': return 49;
        case 'y': return 50;
        case 'z': return 51;
        case '0': return 52;
        case '1': return 53;
        case '2': return 54;
        case '3': return 55;
        case '4': return 56;
        case '5': return 57;
        case '6': return 58;
        case '7': return 59;
        case '8': return 60;
        case '9': return 61;
        case '+': return 62;
        case '/': return 63;
        default: throw new Error("invalid base64 character: " + s);
    }
}
//# sourceMappingURL=vdk_base64.js.map