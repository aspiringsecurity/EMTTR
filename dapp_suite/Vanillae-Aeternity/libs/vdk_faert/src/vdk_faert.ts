/**
 * FÆRT: Fast Æternity Recovery Text
 *
 * Reference: https://gitlab.com/zxq9/passgas/-/blob/83607fedb08be5dfd03210e331f9c76125bb3467/fullofbeans
 *
 * @module
 */

export {
    encode,
    decode,
    byte_of_word,
    check_word,
    check_byte,
    words
}

import * as safe from './jex_include/local-vdk_safe-0.1.0/dist/vdk_safe.js';



/**
 * Encode a bytestring into FÆRT
 */
function
encode
    (bytes : Uint8Array)
    : string
{
    // initial accumulator
    let words_acc  : Array<string> = [];

    // go along each byte and look up the word
    // add it to the accumulator
    for (let this_byte of bytes)
    {
        let this_word = words[this_byte];
        words_acc.push(this_word);
    }

    // prepend check word to phrase
    // yes craig it would be faster to do the check byte inline
    // the usage case here is 64 byte strings
    // double pass is not a big deal
    return check_word(bytes) + ' ' + words_acc.join(' ');
}



/**
 * Decode a FAERT string into bytes
 */
function
decode
    (faert : string)
    : safe.Safe<Uint8Array, string>
{
    let all_words        : Array<string> = faert.split(' ');
    let input_check_word : string        = all_words[0];
    let input_words      : Array<string> = all_words.slice(1)

    // accumulator
    let computed_bytes : Array<number> = [];

    // loop over words and figure out accumulator
    for (let this_input_word of input_words)
    {
        let maybe_this_byte : safe.Safe<number, string> = byte_of_word(this_input_word);
        // if the word is an allowable word, add it to the accumulator
        if (maybe_this_byte.ok)
        {
            let this_byte : number = maybe_this_byte.result;
            computed_bytes.push(this_byte);
        }
        // error case, propagate the error up the call chain
        else
            return maybe_this_byte;
    }

    // at this point, we can assume we correctly decoded all words
    // compute the check byte
    let computed_bytes_u8s  : Uint8Array = new Uint8Array(computed_bytes);
    let computed_check_word : string = check_word(computed_bytes_u8s);

    // check if it is correct
    // if so, return the computed bytes
    if (computed_check_word === input_check_word)
        return safe.ok(computed_bytes_u8s);
    // otherwise, return an error
    else
        return safe.error('checksum failure! computed check word: ' + computed_check_word + '; input check word: ' + input_check_word);
}



/**
 * given a word, find its index
 */
function
byte_of_word
    (word: string)
    : safe.Safe<number, string>
{
    for (let i=0; i<=255; i++)
    {
        if (word === words[i])
            return safe.ok(i);
    }
    return safe.error('invalid word: ' + word);
}



/**
 * compute the check word of an array
 */
function
check_word
    (bytes: Uint8Array)
    : string
{
    return words[check_byte(bytes)];
}


/**
 * given an array, compute the xor of all the bytes in the array
 */
function
check_byte
    (bytes: Uint8Array)
    : number
{
    let check_byte = 0;
    for (let this_byte of bytes)
        check_byte ^= this_byte;
    return check_byte;
}



let words = [
    "able",
    "abuse",
    "acquire",
    "adjust",
    "agent",
    "air",
    "alert",
    "alpha",
    "anger",
    "answer",
    "any",
    "argue",
    "around",
    "assume",
    "asthma",
    "aunt",
    "awkward",
    "balcony",
    "barrel",
    "because",
    "behave",
    "bicycle",
    "bike",
    "blind",
    "blouse",
    "bonus",
    "bottom",
    "breeze",
    "brother",
    "bubble",
    "burger",
    "butter",
    "can",
    "cannon",
    "cargo",
    "catalog",
    "caught",
    "century",
    "chaos",
    "chicken",
    "churn",
    "cinnamon",
    "clever",
    "clock",
    "clown",
    "collect",
    "conduct",
    "convince",
    "correct",
    "cradle",
    "crane",
    "crime",
    "cross",
    "crystal",
    "curve",
    "daughter",
    "debris",
    "decrease",
    "deny",
    "deputy",
    "despair",
    "diesel",
    "dinner",
    "distance",
    "document",
    "donate",
    "drama",
    "drink",
    "dutch",
    "earth",
    "educate",
    "elbow",
    "employ",
    "endless",
    "enlist",
    "enter",
    "equal",
    "eternal",
    "example",
    "exhibit",
    "exotic",
    "faculty",
    "famous",
    "fault",
    "feature",
    "fiber",
    "filter",
    "firm",
    "flame",
    "flush",
    "follow",
    "forward",
    "frame",
    "fringe",
    "future",
    "game",
    "gate",
    "gift",
    "glare",
    "glow",
    "goat",
    "grab",
    "grief",
    "guilt",
    "hand",
    "hawk",
    "health",
    "hen",
    "hire",
    "honey",
    "hover",
    "hurry",
    "hybrid",
    "impose",
    "inch",
    "inherit",
    "injury",
    "install",
    "iron",
    "jazz",
    "joke",
    "just",
    "kit",
    "kitchen",
    "language",
    "laundry",
    "leader",
    "lend",
    "leopard",
    "license",
    "live",
    "lobster",
    "lounge",
    "magnet",
    "mail",
    "mansion",
    "mass",
    "math",
    "media",
    "message",
    "metal",
    "misery",
    "mix",
    "more",
    "mountain",
    "museum",
    "mutual",
    "narrow",
    "nerve",
    "next",
    "note",
    "obey",
    "obtain",
    "offer",
    "once",
    "orange",
    "ostrich",
    "over",
    "owner",
    "palace",
    "patch",
    "pave",
    "pen",
    "phrase",
    "piece",
    "pizza",
    "plunge",
    "polar",
    "pool",
    "power",
    "pretty",
    "private",
    "protect",
    "pulp",
    "purity",
    "quit",
    "quote",
    "raise",
    "razor",
    "recall",
    "region",
    "relief",
    "rent",
    "rescue",
    "review",
    "ride",
    "risk",
    "roof",
    "rough",
    "saddle",
    "salmon",
    "sand",
    "scene",
    "scrub",
    "season",
    "seek",
    "series",
    "shed",
    "shoe",
    "sick",
    "silent",
    "situate",
    "skill",
    "slide",
    "slush",
    "snack",
    "solar",
    "soul",
    "special",
    "sphere",
    "spot",
    "spy",
    "stairs",
    "stereo",
    "strategy",
    "stuff",
    "success",
    "sunny",
    "surge",
    "swear",
    "symptom",
    "tail",
    "ten",
    "tent",
    "theme",
    "thumb",
    "tiny",
    "toe",
    "tooth",
    "topic",
    "trade",
    "trash",
    "trophy",
    "truly",
    "tumble",
    "typical",
    "uncle",
    "unfair",
    "until",
    "upper",
    "useless",
    "van",
    "venue",
    "video",
    "visa",
    "vocal",
    "walk",
    "waste",
    "wedding",
    "weekend",
    "wide",
    "winner",
    "wise",
    "wood",
    "wrist",
    "zero"
]
