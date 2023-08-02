/**
 * Node API constructor/deconstructor
 *
 * This is similar to serialization/deserialization, but not quite the same
 * thing. It converts back and forth between different forms of
 * "api-serialized" data.
 *
 * References:
 * 1. https://github.com/aeternity/protocol/blob/master/serializations.md
 * 2. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
 *
 * ## General type rules
 *
 * ```
 * ERLANG TYPE     |   JS TYPE
 * -------------------------------
 * integer         |   bigint
 * list            |   Array
 * binary          |   Uint8Array
 * ```
 *
 * # Example
 *
 * We start with the string `tx_+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==`.
 *
 * We can tell by the `tx_` prefix that this represents transaction data of
 * some sort. But the rest of the data is totally opaque. The task of this
 * module is to "humanize" that `tx_...` string and show what data is contained
 * in the rest of it.
 *
 * The remainder of the string is a base64-encoded bytestring
 *
 * ```erlang
 * 3> io:format("~tw~n", [base64:decode(<<"+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==">>)]).
 * <<248,88,12,1,161,1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,118,68,68,244,48,85,218,93,85,120,164,154,55,90,250,24,220,161,1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,89,203,191,216,108,88,186,7,3,234,139,205,10,184,167,108,10,134,15,38,245,97,200,0,0,9,135,104,97,105,110,97,110,97,3,182,66,215>>
 * ```
 *
 * That bytestring contains data encoded using Ethereum's RLP codec. Luckily, I
 * wrote an RLP decoder.  RLP has two types of data: binaries, and
 * arbitrary-depth (possibly empty) lists of binaries.
 *
 * ```erlang
 * -type decoded_data() :: binary() | [decoded_data()].
 *
 * -spec decode(RLP) -> {Data, Rest}
 *     when RLP  :: binary(),
 *          Data :: decoded_data(),
 *          Rest :: binary().
 * ```
 *
 * ```erlang
 * 2> rlp:decode(base64:decode(<<"+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==">>)).
 * {[<<"\f">>,
 *   <<1>>,
 *   <<1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,
 *     118,68,68,244,48,85,218,93,85,...>>,
 *   <<1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,
 *     89,203,191,216,108,88,186,...>>,
 *   <<"\n">>,
 *   <<15,38,245,97,200,0>>,
 *   <<0>>,
 *   <<"\t">>,<<"hainana">>],
 *  <<3,182,66,215>>}
 * ```
 *
 * As expected, we get back the return tuple `{Data, Rest}`. `Rest` is the double-sha256 of the beginning
 *
 * ```erlang
 * 3> X = base64:decode(<<"+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==">>).
 * <<248,88,12,1,161,1,201,99,126,70,0,231,171,59,194,230,
 *   127,198,64,232,35,118,68,68,244,48,85,218,93,...>>
 * 4> SizeX = byte_size(X).
 * 94
 * 6> <<RLPEncodedData:(SizeX - 4)/binary, Hash/binary>> = X.
 * <<248,88,12,1,161,1,201,99,126,70,0,231,171,59,194,230,
 *   127,198,64,232,35,118,68,68,244,48,85,218,93,...>>
 * 10> <<Check:4/binary, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, RLPEncodedData)).
 * <<3,182,66,215,195,99,112,99,25,7,84,31,151,188,149,81,
 *   189,184,82,207,164,68,128,43,11,174,236,59,77,...>>
 * 11> Hash.
 * <<3,182,66,215>>
 * 12> Check.
 * <<3,182,66,215>>
 * ```
 *
 * What we're really interested in is `Data`
 *
 * ```erlang
 * 14> {Data, _} = rlp:decode(X).
 * {[<<"\f">>,
 *   <<1>>,
 *   <<1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,
 *     118,68,68,244,48,85,218,93,85,...>>,
 *   <<1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,
 *     89,203,191,216,108,88,186,...>>,
 *   <<"\n">>,
 *   <<15,38,245,97,200,0>>,
 *   <<0>>,
 *   <<"\t">>,<<"hainana">>],
 *  <<3,182,66,215>>}
 * ```
 *
 * `Data` is a list. The first field `<<"\f">>` is meant to be an integer which
 * tells us what type of data this is.
 *
 * ```erlang
 * 16> $\f.
 * 12
 * ```
 *
 * If we look at our table
 * (https://github.com/aeternity/protocol/blob/master/serializations.md#table-of-object-tags),
 * we see that a value of `12` is a spend transaction.
 *
 * The second field `<<1>>` tells us the "version" of the field orderings,
 * which we can ignore for now.
 *
 * The remaining fields are the fields of a spend transaction (https://github.com/aeternity/protocol/blob/master/serializations.md#spend-transaction)
 *
 * ```erlang
 * [ <sender>    :: id()        % <<1,201,99,126,...>    "=" "ak_2XhCkjzTwcq1coXSSzHJoMZkUzTwnjH88zmPGkkowUsFNTo9UE"
 * , <recipient> :: id()        % <<1,123,102,230,...>   "=" "ak_wM8yFU8eSETXU7VSN48HMDmevGoCMiuveQZgkPuRn1nTiRqyv"
 * , <amount>    :: int()       % <<"\n">>               "=" 10
 * , <fee>       :: int()       % <<15,38,245,97,200,0>> "=" 16_660_000_000_000
 * , <ttl>       :: int()       % <<0>>                  "=" 0
 * , <nonce>     :: int()       % <<"\t">>               "=" 9
 * , <payload>   :: binary()    % <<"hainana">>          "=" "hainana"
 * ]
 * ```
 *
 * Our task here is to be able to pull apart the "tx_..." string into its fields.
 *
 * Converting the binaries to integers is pretty trivial. The only mildly
 * annoying thing is the `id` type.
 *
 * `id`s have two fields: a single-byte prefix which says which type of ID it
 * is. In this case, both `id`s have a prefix of `1`, which means they are both
 * normal accounts (hence the `ak_` prefix on the "api-encoded" id).  The other
 * options are oracles (prefix `4`/`ok_`), contracts (prefix `5`/`ct_`), or
 * names (prefix `2`/`nm_`)
 *
 * To "api-encode" the name, we first pick the appropriate prefix based on the
 * first byte (in this case `1 -> "ak_"). The remaining 32 bytes are then
 * double-SHA'd to get the 4-byte check suffix
 *
 * ```erlang
 * 30> SenderBytes = lists:nth(3, Data).
 * <<1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,
 *   118,68,68,244,48,85,218,93,85,120,164,154,55,...>>
 * 31> <<1, SenderAddrBytes/binary>> = SenderBytes.
 * <<1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,
 *   118,68,68,244,48,85,218,93,85,120,164,154,55,...>>
 * 32> DoubleSha = fun(Bytes) -> <<Foo:4/binary, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, Bytes)), Foo end.
 * #Fun<erl_eval.44.97283095>
 * 33> "ak_" ++ b58:enc(<<SenderAddrBytes/binary, (DoubleSha(SenderAddrBytes))/binary>>).
 * "ak_2XhCkjzTwcq1coXSSzHJoMZkUzTwnjH88zmPGkkowUsFNTo9UE"
 * 34> RecipBytes = lists:nth(4, Data).
 * <<1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,
 *   89,203,191,216,108,88,186,7,3,234,139,205,...>>
 * 35> <<1, RecipAddrBytes/binary>> = RecipBytes.
 * <<1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,
 *   89,203,191,216,108,88,186,7,3,234,139,205,...>>
 * 36> "ak_" ++ b58:enc(<<RecipAddrBytes/binary, (DoubleSha(RecipAddrBytes))/binary>>).  
 * "ak_wM8yFU8eSETXU7VSN48HMDmevGoCMiuveQZgkPuRn1nTiRqyv"
 * ```
 *
 * ```js
 * > anth.deconstruct("tx_+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==")
 * {tag     : 'SpendTx',
 *  version : 1n,
 *  fields  : {sender    : "ak_2XhCkjzTwcq1coXSSzHJoMZkUzTwnjH88zmPGkkowUsFNTo9UE",
 *             recipient : "ak_wM8yFU8eSETXU7VSN48HMDmevGoCMiuveQZgkPuRn1nTiRqyv",
 *             amount    : 10n,
 *             fee       : 16660000000000n,
 *             ttl       : 0n,
 *             nonce     : 9n,
 *             payload   : Uint8Array([104, 97, 105, 110, 97, 110, 97])}}
 * ```
 *
 * @module
 */

export {
    // types
    tx_str,
    deconstructed_tx,
    // functions
    deconstruct_tx
};

import * as b64 from './b64.js'
import * as bin from './bin.js'
import * as rlp from './rlp.js'


/**
 * Alias type for a `tx_...` string
 */
type tx_str = string;

/**
 * Alias type for a `sg_...` string
 */
type sg_str = string;

/**
 * types of decoded tx we currently support
 * @internal
 */
type tx_type_str
    = 'SignedTx'
    | 'SpendTx'
    | 'ContractCreateTx'
    | 'ContractCallTx';


/**
 * Results of deconstruct_tx
 */
type deconstructed_tx
    = {type    : 'SignedTx',
       version : bigint,
       fields  : fields_SignedTx}
    | {type    : 'SpendTx',
       version : bigint,
       fields  : fields_SpendTx}
    | {type    : 'ContractCreateTx',
       version : bigint,
       fields  : fields_ContractCreateTx}
    | {type    : 'ContractCallTx'
       version : bigint,
       fields  : fields_ContractCallTx};

/**
 * Convenient type alias
 *
 * @internal
 */
type rlpdata = rlp.decoded_data;


/**
 * Fields types
 */
type fields
    = fields_SignedTx
    | fields_SpendTx
    | fields_ContractCreateTx
    | fields_ContractCallTx;

type fields_SignedTx
    = {signatures  : Array<sg_str>,
       transaction : tx_str};

type fields_SpendTx
    = {sender    : string,
       recipient : string,
       amount    : bigint,
       fee       : bigint,
       ttl       : bigint,
       nonce     : bigint,
       payload   : Uint8Array};

/**
 * Deconstruct a Tx
 */
function
deconstruct_tx
    (tx_str: tx_str)
    : deconstructed_tx
{
    let b64_str        : string         = tx_str.slice(3);                         // tx_[...] -> [...]
    let tx_rlp_encoded : Uint8Array     = b64.decode(b64_str);                     // [...] -> bytes
    let tx_data        : Array<rlpdata> = shasha_rlp_decode_list(tx_rlp_encoded);  // decode data and check the double-sha thing
    let tx_type        : bigint         = bin.bytes_to_bigint(tx_data[0]);         // get a bigint
    let tts            : tx_type_str    = tx_type_str(tx_type);
    let tx_version     : bigint         = bin.bytes_to_bigint(tx_data[1]);
    let tx_fields      : fields         = deconstruct_fields(tts, tx_version, tx_data.slice(2));
    return {type    : tts,
            version : tx_version,
            fields  : tx_fields};
}


/**
 * Data that's "api-encoded" goes through the following stages:
 *
 * 1. data structure -> rlp decode data (arbitrary-depth [possibly 0] list of bytestrings)
 * 2. rlp decode data -> bytestring
 * 3. bytestring -> <<Bytestring/binary, Hash:4/binary>>
 * 4. HashedBytestring -> base64/base58 string encoding
 * 5. Add string prefix
 *
 * This function undoes step 3 and step 2, returns back the rlp decode data
 *
 * FIXME: Does not check double-sha (yet); need to figure out way to handle hash failures
 * FIXME: No good way to handle failure cases
 *
 * @internal
 */
function
shasha_rlp_decode_list
    (hashed_bs : Uint8Array)
    : Array<rlpdata>
{
    let len   = hashed_bs.length;
    let bytes = hashed_bs.slice(0, len - 4);
    let {decoded_data} = rlp.decode(bytes);
    return (decoded_data as Array<rlpdata>);
}



/**
 * Convert an object tag that's a type of transaction to the type string
 *
 * See: https://github.com/aeternity/protocol/blob/master/serializations.md#table-of-object-tags
 *
 * @internal
 */
function
tx_type_str
    (tx_type_int : bigint)
    : tx_type_str
{
    switch (tx_type_int)
    {
        case 11n: return 'SignedTx';
        case 12n: return 'SpendTx';
        case 42n: return 'ContractCreateTx';
        case 43n: return 'ContractCallTx';
        default: throw new Error('invalid transaction type: ' + tx_type_int);
    }
}



/**
 * Given an array of data decoded from RLP, convert it to the fields, as
 * appropriate as given by the tx type string and the version
 */
function
deconstruct_fields
    (tx_type_str  : tx_type_str,
     tx_version   : bigint,
     tx_rawfields : Array<rlpdata>)
    : fields
 {
    switch (tx_type_str)
    {
        // case 'SignedTx'         : return deconstruct_fields_SignedTx(tx_rawfields);
        case 'SpendTx'          : return deconstruct_fields_SpendTx(tx_rawfields);
        // case 'ContractCreateTx' : return deconstruct_fields_ContractCreateTx(tx_rawfields);
        // case 'ContractCallTx'   : return deconstruct_fields_ContractCallTx(tx_rawfields);
        default                 : throw new Error('invalid tx type str: ' + tx_type_str);
    }
 }


// TODO: do all this in Erlang
 function
 deconstruct_fields_SpendTx
    (fields: Array<rlpdata>)
    : fields_SpendTx
{
    let sender_bytes  = fields[0];
    let recip_bytes   = fields[1];
    let amount_bytes  = fields[2];
    let fee_bytes     = fields[3];
    let ttl_bytes     = fields[4];
    let nonce_bytes   = fields[5];
    let payload_bytes = fields[6];
    return {sender    : encode_id(sender_bytes),
            recipient : encode_id(sender_bytes),
            amount    : bin.bytes_to_bigint(amount_bytes),
            fee       : bin.bytes_to_bigint(fee_bytes),
            ttl       : bin.bytes_to_bigint(ttl_bytes),
            nonce     : bin.bytes_to_bigint(nonce_bytes),
            payload   : bin.bytes_to_bigint(payload_bytes)};
}


/**
 * Convert a binary account/name/etc binary id into the appropriate type of string
 *
 * @internal
 */
function
encode_id
    (id: Uint8Array)
    : string
{
    throw new Error('nyi');
}

/*
FIXME:
1. work out all this in Erlang to clear conceptual goo
2. think about how i want type safety etc to work
3. think about a language to assert that the data has the correct shape to it
4. get some examples working in Erlang
5. convert erlang code back to ts
*/
