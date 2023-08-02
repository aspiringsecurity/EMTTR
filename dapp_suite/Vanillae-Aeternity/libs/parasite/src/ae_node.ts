/********************************************************************
** Node API: functions for talking to an Aeternity node
**
** In the future, everything that this module does will be replaced
** by the backend.
**
** Functions should be sorted in alphabetical order.
**
** Names are what they are in the documentation
**
** Useful links:
**
**  - HTML API docs : https://api-docs.aeternity.io/
**  - YAML API docs : https://github.com/aeternity/aeternity/blob/master/apps/aehttp/priv/swagger.yaml
**
********************************************************************/


import * as net         from './net.js'
import * as ae_compiler from './ae_compiler.js'

//-------------------------------------------------------------------
// CONSTANTS
//-------------------------------------------------------------------

export const MIN_FEE          = 16660000000000;
export const MIN_CONTRACT_FEE = 79080000000000;
export const MIN_GAS_PRICE    =     1000000000;
export const URL_MAINNET      = "https://mainnet.aeternity.io/v3";
export const URL_TESTNET      = "https://testnet.aeternity.io/v3";


//-------------------------------------------------------------------
// CANONICAL TYPES ("MODELS") FROM THE DOCUMENTATION
//
// All of these names are as given in the documentation except `Error`
// (which is a reserve term in JS), renamed to `ErrorReason`
//-------------------------------------------------------------------

// Error
//
// Docs: https://api-docs.aeternity.io/#/definitions/Error
// Docs: https://github.com/aeternity/aeternity/blob/v6.4.0/apps/aehttp/priv/swagger.yaml#L3168-L3172
type ErrorReason = {reason: string};

type Tx = {tx: string};



//-------------------------------------------------------------------
// FUNCTIONS
//
// The names here follow their names in the documentation
//-------------------------------------------------------------------


//-------------------------------------------------------------------
// GetAccountNextNonce: /accounts/{pubkey}/next-nonce
//
// Docs: https://api-docs.aeternity.io/#/account/GetAccountNextNonce
//
// > Get an account's next nonce; This is computed according to
// > whatever is the current account nonce and what transactions are
// > currently present in the transaction pool
//-------------------------------------------------------------------

type GetAccountNextNonce_params = {pubkey    : string,
                                   strategy? : "max" | "continuity"};

type GetAccountNextNonce_ret    = {next_nonce: string};


async function
GetAccountNextNonce(endpoint_url : string,
                    params       : GetAccountNextNonce_params)
    : Promise< GetAccountNextNonce_ret
             | ErrorReason>
{
    let pubkey = params.pubkey;
    let url    = `${endpoint_url}/accounts/${pubkey}/next-nonce`;

    // if the "strategy" field is present, add it as a ?strategy=x
    // option
    //
    // note if the field is absent from `params`, then
    // `params.strategy` will be `undefined`, which in js whacko
    // world is "falsy"
    let strategy = params.strategy;
    if (strategy)
    {
        let addon = `?strategy=${strategy}`;
        url += addon;
    }

    // irrespective of the response code, this is what we return
    // so branching is gay
    let ret = await net.get_json(url);
    return ret;
}



//-------------------------------------------------------------------
// PostContractCreate
//
// Docs: https://api-docs.aeternity.io/#/contract/PostContractCreate
//-------------------------------------------------------------------

// > Get a contract_create transaction object


type ContractCreateTx = {owner_id    : string,
                         nonce?      : number,
                         code        : string,
                         vm_version  : number,
                         abi_version : number,
                         deposit     : number,
                         amount      : number,
                         gas         : number,
                         gas_price   : number,
                         fee         : number,
                         ttl?        : number,
                         call_data   : string};



async function
PostContractCreate(endpoint_url : string,
                   body_obj     : ContractCreateTx)
    : Promise<Response>
{
    // console.log('body_obj', body_obj);
    let url = `${endpoint_url}/debug/contracts/create`;
    let ret = await net.post_json_response(url, body_obj);
    return ret;
}



async function
create_contract(whoami    : string,
                code      : string,
                filename  : string,
                init_args : Array<string>)
    : Promise<Response>
{
    let code_resp = await ae_compiler.CompileContract(code, filename);
    let code_json = await code_resp.json();
    let bytecode  = code_json.bytecode;

    let calldata_resp = await ae_compiler.EncodeCalldata(code, filename, "init", init_args);
    // assert(calldata_resp.ok);
    let calldata_json = await calldata_resp.json();
    let calldata      = calldata_json.calldata;

    let cctx: ContractCreateTx =
        {owner_id    : whoami,
         code        : bytecode,
         vm_version  : 7,
         abi_version : 3,
         deposit     : 0,
         amount      : 0,
         gas         : 25000,
         gas_price   : 1*MIN_GAS_PRICE,
         fee         : 1*MIN_CONTRACT_FEE,
         call_data   : calldata};

    let ret = await PostContractCreate(URL_TESTNET, cctx);

    return ret;
}



//-------------------------------------------------------------------
// PostSpend: /debug/transactions/spend
//
// Docs:
//  - Input type  : https://api-docs.aeternity.io/#/definitions/SpendTx
//  - Return type : https://api-docs.aeternity.io/#/definitions/Tx
//  - Function    : https://api-docs.aeternity.io/#/transaction/PostSpend
//
// > Get a spend transaction object
//-------------------------------------------------------------------

//-------------------------------------------------------------------
// SpendTx
//
// Docs: https://api-docs.aeternity.io/#/definitions/SpendTx
// Docs: https://github.com/aeternity/aeternity/blob/v6.4.0/apps/aehttp/priv/swagger.yaml#L2187-L2209
//-------------------------------------------------------------------
type SpendTx =
    {recipient_id : string,
     amount       : number,
     fee          : number,
     ttl?         : number,
     sender_id    : string,
     nonce?       : number,
     payload      : string};



async function
PostSpend(endpoint_url : string,
          body_obj     : SpendTx)
    : Promise<Tx | ErrorReason>
{
    let url = `${endpoint_url}/debug/transactions/spend`;
    let ret = await net.post_json(url, body_obj);
    return ret;
}



//-------------------------------------------------------------------
// PostTransaction
//
// Docs: https://api-docs.aeternity.io/#/contract/PostTransaction
//
// > Post a new transaction
//-------------------------------------------------------------------

async function
PostTransaction(endpoint_url : string,
                body_obj     : Tx)
    : Promise<Response>
{
    // console.log('body_obj', body_obj);
    let url = `${endpoint_url}/transactions`;
    let ret = await net.post_json_response(url, body_obj);
    return ret;
}



//-------------------------------------------------------------------
// GetTransactionInfoByHash
//
// Docs: https://api-docs.aeternity.io/#/contract/GetTransactionInfoByHash
//-------------------------------------------------------------------

async function
GetTransactionInfoByHash(endpoint_url : string,
                         hash         : string)
    : Promise<Response>
{
    // console.log('body_obj', body_obj);
    let url = `${endpoint_url}/transactions/${hash}/info`;
    let ret = await net.get_json_response(url);
    return ret;
}


//-------------------------------------------------------------------
// EXPORTS
//-------------------------------------------------------------------

// type exports
export type {
    ErrorReason,
    Tx,
    SpendTx,
    ContractCreateTx
};

export {
    GetAccountNextNonce,
    PostContractCreate,
    create_contract,
    PostSpend,
    PostTransaction
};
