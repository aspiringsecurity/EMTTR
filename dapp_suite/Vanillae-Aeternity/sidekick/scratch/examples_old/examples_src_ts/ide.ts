// TODO: make this all into top-level calls

import * as ae_compiler from './parasite/ae_compiler.js';
import * as ae_node     from './parasite/ae_node.js';
import * as sidekick    from '../sidekick_dist/dist_js/sidekick.js';


function
value_of_id
    (str : string)
    : string
{
    // angle bracket jizz is type inference jizz; does not change
    // runtime behavior;
    //
    // https://stackoverflow.com/questions/12989741/the-property-value-does-not-exist-on-value-of-type-htmlelement
    return (<HTMLInputElement>document.getElementById(str)).value;
}

// COMPILE

async function
user_clicked_insert
    ()
    : Promise<void>
{
    let template_name = value_of_id("template-name");
    let template_resp = await fetch(`http://localhost:8001/contract-examples/${template_name}`);
    let template_code = await template_resp.text();

    // Angle brackets are type inference jizz
    //
    // https://stackoverflow.com/questions/12989741/the-property-value-does-not-exist-on-value-of-type-htmlelement
    (<HTMLInputElement>document.getElementById('the-filename')).value = template_name;
    // bang turns off null warning
    document.getElementById('the-code')!.innerHTML = template_code;
}



// COMPILE
async function
user_clicked_compile
    ()
    : Promise<void>
{
    let code     = value_of_id('the-code');
    let filename = value_of_id('the-filename');

    console.log('code:');
    console.log(code);

    let response = await ae_compiler.CompileContract(code, filename);
    let status_n = response.status;
    let status_t = response.statusText;
    let result_json = await response.json();

    console.log('status_n', status_n);
    console.log('status_t', status_t);
    console.log('result_t', result_json);

    // bang turns off typescript possibly null warning
    document.getElementById('compile-result-status')!.innerText = `${status_n} ${status_t}`;
    document.getElementById('compile-result-json')!.innerHTML = sidekick.pf(result_json);
}



function
user_clicked_clear
    ()
    : void
{
    // bang turns off typescript possibly null warning
    document.getElementById('compile-result-status')!.innerText = '';
    document.getElementById('compile-result-json')!.innerHTML   = '';
}



// ENCODE CALLDATA
async function
user_clicked_encode
    ()
    : Promise<void>
{
    let code      = value_of_id('the-code');
    let filename  = value_of_id('the-filename');
    let fn_name   = value_of_id('EncodeCalldata-function-name');
    let fn_args_s = value_of_id('EncodeCalldata-function-args');
    let fn_args   = JSON.parse(fn_args_s);

    let response =
        await ae_compiler.EncodeCalldata(code,
                                         filename,
                                         fn_name,
                                         fn_args);
    let status_n = response.status;
    let status_t = response.statusText;
    let result_json = await response.json();

    console.log('status_n', status_n);
    console.log('status_t', status_t);
    console.log('result_t', result_json);

    // bang turns off null warnings in tsc
    document.getElementById('EncodeCalldata-result-status')!.innerText = `${status_n} ${status_t}`;
    document.getElementById('EncodeCalldata-result-json')!.innerHTML = sidekick.pf(result_json);
}



function
user_clicked_encode_clear
    ()
    : void
{
    // bang turns off null warnings in tsc
    document.getElementById('EncodeCalldata-result-status')!.innerText = '';
    document.getElementById('EncodeCalldata-result-json')!.innerText   = '';
}



// CONNECT TO SUPERHERO
async function
user_clicked_connect
    (skl : sidekick.Skylight)
    : Promise<void>
{
    let wallet_addr = await sidekick.handshake_def(skl);

    // put it in the thing
    // bang turns off maybe null typescript warning
    document.getElementById('user-wallet-address')!.innerHTML = wallet_addr;
}



async function
user_clicked_create
    (skl : sidekick.Skylight)
    : Promise<void>
{
    // create_contract(whoami    : string,
    //                 code      : string,
    //                 filename  : string,
    //                 init_args : Array<string>)
    let whoami    = await sidekick.address(skl, sidekick.TIMEOUT_DEF_ADDRESS);
    let code      = value_of_id('the-code');
    let filename  = value_of_id('the-filename');
    let fn_args_s = value_of_id('deploy-function-args');
    let fn_args   = JSON.parse(fn_args_s);

    let resp          = await ae_node.create_contract(whoami, code, filename, fn_args);
    let resp_status_n = await resp.status;
    let resp_status_t = await resp.statusText;
    let resp_json     = await resp.json();

    // bang tsc null warning jizz
    document.getElementById('contract-create-result-status')!.innerText = `${resp_status_n} ${resp_status_t}`;
    document.getElementById('contract-create-result-json')!.innerHTML = sidekick.pf(resp_json);

    // sign
    let tx_base58str = resp_json.tx;
    let tx_obj       = {tx: tx_base58str};
    let result       = await sidekick.tx_sign_yes_propagate(skl, tx_obj, sidekick.NETWORK_ID_TESTNET, sidekick.TIMEOUT_DEF_SIGN);

    // bang tsc null warning jizz
    document.getElementById('superhero-sign-result-json')!.innerHTML = sidekick.pf(result);

//    // try to post transaction
//    console.log('attempting to post transaction');
//    let posttx_resp          = await ae_node.PostTransaction(ae_node.URL_TESTNET, {tx: result.signedTransaction});
//    let posttx_resp_status_n = await posttx_resp.status;
//    let posttx_resp_status_t = await posttx_resp.statusText;
//    let posttx_resp_json     = await posttx_resp.json();
//
//    // fill in boxes
//    // bang is tsc null checking warning turn off thing
//    document.getElementById('post-transaction-result-status')!.innerHTML = `${posttx_resp_status_n} ${posttx_resp_status_t}`;
//    document.getElementById('post-transaction-result-json')!.innerHTML = helpers.pf(posttx_resp_json);
}


// MAIN
async function
main
    ()
    : Promise<void>
{
    // bang tsc null warning jizz
    // TEMPLATE NAME
    document.getElementById('insert-template-button')!.addEventListener('click', user_clicked_insert);

    // COMPILE
    document.getElementById('compile-button')!.addEventListener('click', user_clicked_compile);
    document.getElementById('compile-clear')!.addEventListener('click', user_clicked_clear);

    // ENCODE CALLDATA
    document.getElementById('EncodeCalldata-button')!.addEventListener('click', user_clicked_encode);
    document.getElementById('EncodeCalldata-button-clear')!.addEventListener('click', user_clicked_encode_clear);


    // DEPLOY

    // this turns on the console logger
    sidekick.snoop_console();
    let my_skylight = await sidekick.start();

    // connect button
    document .getElementById('connect-to-superhero')!
             .addEventListener('click',
                               function () { user_clicked_connect(my_skylight) });
    // create button
    document .getElementById('contract-create-button')!
             .addEventListener('click',
                               function () { user_clicked_create(my_skylight) });

}

main();
