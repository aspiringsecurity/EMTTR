// test cases
import * as cases from './jex_include/local-vdk_tests_cases-0.1.0/dist/cases.js';

// vanillae libs
import * as b64   from './jex_include/local-vdk_base64-0.1.0/dist/vdk_base64.js';
import * as b58   from './jex_include/local-vdk_base58-0.1.0/dist/vdk_base58.js';
import * as rlp   from './jex_include/local-vdk_rlp-0.1.0/dist/vdk_rlp.js';


// TODO: move this to rlp library
function deepeq(dd1: rlp.decoded_data, dd2: rlp.decoded_data): boolean {
    // if both are Uint8Arrays
    if ((dd1 instanceof Uint8Array) && (dd2 instanceof Uint8Array)) {
        return uint8arr_eq(dd1, dd2);
    }
    // if both are Arrays
    else if ((dd1 instanceof Array) && (dd2 instanceof Array)) {
        return arr_eq(dd1, dd2);
    }
    // otherwise
    else {
        return false;
    }
}

function arr_eq(a: Array<rlp.decoded_data>, b: Array<rlp.decoded_data>): boolean {
    // make sure they have the same length
    if (a.length !== b.length) {
        return false;
    }
    // they have the same length now
    else {
        let len : number = a.length;
        // loop over the items
        for (let i = 0;
                 i < len;
                 i++)
        {
            let a_elt : rlp.decoded_data = a[i];
            let b_elt : rlp.decoded_data = b[i];
            let eq    : boolean           = deepeq(a_elt, b_elt);
            // if they are not equal, break and return false
            if (!eq) {
                return false;
            }
        }
        // at the end, if we havent' proven ourselves wrong yet,
        // the arrays are equal
        return true;
    }
}

function uint8arr_eq(a: Uint8Array, b: Uint8Array): boolean {
    // first test if the length
    if (a.length !== b.length) {
        return false;
    }

    // alright so they have the same length
    for (let i = 0; i < a.length; i++) {
        // return false if a[i] != b[i]
        if (a[i] !== b[i]) {
            return false;
        }
    }

    return true;
}


function b64_tests(): void {
    // @ts-ignore ts can't prove to itself that the element exists
    let b64_pre   : HTMLElement = document.getElementById('b64-assershins');
    // @ts-ignore ts can't prove to itself that the element exists
    let b64_casen : HTMLElement = document.getElementById('b64-current-case');
    let case_n    : number      = 1;

    for(let this_case of cases.base64) {
        // i love this type error
        // can't set the inner html to a number
        // but a string plus a number is totally cool
        b64_casen.innerHTML = '' + case_n;
        case_n++;

        let {encoded, decoded} = this_case;
        let my_encoded         = b64.encode(decoded);
        let my_decoded         = b64.decode(encoded);

        let encodes_correctly = (encoded === my_encoded);
        let decodes_correctly = uint8arr_eq(decoded, my_decoded);

        if (!encodes_correctly) {
            b64_pre.innerHTML +=
                '===================================\n' +
                'FAILED CASE: encode\n'                 +
                '===================================\n' +
                'decoded : ' + decoded + '\n'           +
                'expected: ' + encoded + '\n'           +
                'actual  : ' + my_encoded + '\n\n'      ;
        }

        if (!decodes_correctly) {
            b64_pre.innerHTML += 
                '===================================\n' +
                'FAILED CASE: decode\n'                 +
                '===================================\n' +
                'encoded : ' + encoded + '\n'           +
                'expected: ' + decoded + '\n'           +
                'actual  : ' + my_decoded + '\n\n'      ;
        }
    }
}


function b58_tests(): void {
    // @ts-ignore ts can't prove to itself that the element exists
    let b58_pre   : HTMLElement = document.getElementById('b58-assershins');
    // @ts-ignore ts can't prove to itself that the element exists
    let b58_casen : HTMLElement = document.getElementById('b58-current-case');
    let case_n    : number      = 1;

    for(let this_case of cases.base58) {
        // i love this type error
        // can't set the inner html to a number
        // but a string plus a number is totally cool
        b58_casen.innerHTML = '' + case_n;
        case_n++;

        let {encoded, decoded} = this_case;
        let my_encoded         = b58.encode(decoded);
        let my_decoded         = b58.decode(encoded);

        let encodes_correctly = (encoded === my_encoded);
        let decodes_correctly = uint8arr_eq(decoded, my_decoded);

        if (!encodes_correctly) {
            b58_pre.innerHTML +=
                '===================================\n' +
                'FAILED CASE: encode\n'                 +
                '===================================\n' +
                'decoded : ' + decoded + '\n'           +
                'expected: ' + encoded + '\n'           +
                'actual  : ' + my_encoded + '\n\n'      ;
        }

        if (!decodes_correctly) {
            b58_pre.innerHTML +=
                '===================================\n' +
                'FAILED CASE: decode\n'                 +
                '===================================\n' +
                'encoded : ' + encoded + '\n'           +
                'expected: ' + decoded + '\n'           +
                'actual  : ' + my_decoded + '\n\n'      ;
        }
    }
}


function rlp_tests(): void {
    // @ts-ignore ts can't prove to itself that the element exists
    let rlp_pre   : HTMLElement = document.getElementById('rlp-assershins');
    // @ts-ignore ts can't prove to itself that the element exists
    let rlp_casen : HTMLElement = document.getElementById('rlp-current-case');
    let case_n    : number      = 1;

    for(let this_case of cases.rlp) {
        // i love this type error
        // can't set the inner html to a number
        // but a string plus a number is totally cool
        rlp_casen.innerHTML = '' + case_n;
        case_n++;

        let {encoded, decoded} = this_case;
        let my_encoded         = rlp.encode(decoded);
        let my_decoded         = rlp.decode(encoded).decoded_data;

        let encodes_correctly = uint8arr_eq(encoded, my_encoded);
        let decodes_correctly = deepeq(decoded, my_decoded);

        if (!encodes_correctly) {
            rlp_pre.innerHTML +=
                '===================================\n' +
                'FAILED CASE: encode\n'                 +
                '===================================\n' +
                'decoded : ' + decoded + '\n'           +
                'expected: ' + encoded + '\n'           +
                'actual  : ' + my_encoded + '\n\n'      ;
        }

        if (!decodes_correctly) {
            rlp_pre.innerHTML +=
                '===================================\n' +
                'FAILED CASE: decode\n'                 +
                '===================================\n' +
                'encoded : ' + encoded + '\n'           +
                'expected: ' + decoded + '\n'           +
                'actual  : ' + my_decoded + '\n\n'      ;
        }
    }
}



function main(): void {
    document.getElementById('b64-total-cases')!.innerHTML = '' + cases.base64.length;
    document.getElementById('b64-go')!.onclick            = b64_tests;

    document.getElementById('b58-total-cases')!.innerHTML = '' + cases.base58.length;
    document.getElementById('b58-go')!.onclick            = b58_tests;

    document.getElementById('rlp-total-cases')!.innerHTML = '' + cases.rlp.length;
    document.getElementById('rlp-go')!.onclick            = rlp_tests;
}

main();
