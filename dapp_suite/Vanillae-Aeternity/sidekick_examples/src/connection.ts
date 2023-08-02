import * as ae_node from './jex_include/local-parasite-0.2.0/dist/ae_node.js';
import * as awcp    from './jex_include/local-awcp-0.2.1/dist/awcp.js';
import * as sk      from './jex_include/local-sidekick-0.2.1/dist/sidekick.js';

var pv_address: string;
var pv_spendtx_base64: string;

function
set_pv_address
    (new_address: string)
    : void
{
    pv_address = new_address;
    for (let elt of document.getElementsByClassName('pv-address'))
    {
        elt.innerHTML = new_address;
    }
}

function
set_pv_spendtx_base64
    (new_address: string)
    : void
{
    pv_spendtx_base64 = new_address;
    for (let elt of document.getElementsByClassName('pv-spendtx-base64'))
    {
        elt.innerHTML = new_address;
    }
}



async function
detect
    (logger: sk.Logger)
    : Promise<void>
{
    let h4 = document.getElementById("detected")!;
    let pre = document.getElementById("detect-info")!;

    h4.innerHTML = 'detecting...';
    h4.style.color = 'GoldenRod';

    // try to detect the wallet
    // will fail on timeout error
    // console logger
    let maybe_wallet_info =//: sk.Safe<awcp.EventData_W2A_connection_announcePresence, sk.TimeoutError> =
            await sk.detect(sk.TIMEOUT_DEF_DETECT_MS, "failed to detect wallet", logger);

    console.log(maybe_wallet_info);

    // ok means wallet was detected
    if (maybe_wallet_info.ok)
    {
        h4.innerHTML   = "detected";
        h4.style.color = "green";
        pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
    }
    else
    {
        h4.innerHTML = "error";
        h4.style.color = "crimson";
        pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
    }
}

async function
connect
    (logger: sk.Logger)
    : Promise<void>
{
    // this is not working
    // I am creating and posting an identical message to the working example
    // it must be a mismatch in the extraneous properties of the MessageEvent that is causing the problem
    let h4 = document.getElementById("connected")!;
    let pre = document.getElementById("connect-info")!;

    h4.innerHTML = 'connecting...';
    h4.style.color = 'GoldenRod';

    // try to connect to the wallet
    // will fail on timeout error
    // console logger
    let maybe_wallet_info = await sk.connect(
            'ske-connect-1',
            {name: 'sidekick examples',
             version: 1},
            sk.TIMEOUT_DEF_CONNECT_MS,
            "failed to connect to wallet",
            logger
        );

    console.log(maybe_wallet_info);

    // ok means wallet was connected
    if (maybe_wallet_info.ok)
    {
        h4.innerHTML   = "connected";
        h4.style.color = "green";
        pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
    }
    else
    {
        h4.innerHTML = "error";
        h4.style.color = "crimson";
        pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
    }
}

async function
address
    (logger: sk.Logger)
    : Promise<void>
{
    // this is not working
    // I am creating and posting an identical message to the working example
    // it must be a mismatch in the extraneous properties of the MessageEvent that is causing the problem
    let h4 = document.getElementById("addressed")!;
    let pre = document.getElementById("address-info")!;

    h4.innerHTML = 'addressing...';
    h4.style.color = 'GoldenRod';

    // try to address to the wallet
    // will fail on timeout error
    // console logger
    let maybe_wallet_info = await sk.address(
            'ske-address-1',
            {type: 'subscribe',
             value: 'connected'},
            sk.TIMEOUT_DEF_ADDRESS_MS,
            "failed to address to wallet",
            logger
        );

    console.log(maybe_wallet_info);

    // ok means wallet was addressed
    if (maybe_wallet_info.ok)
    {
        h4.innerHTML   = "addressed";
        h4.style.color = "green";
        pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
        // update global variable
        let the_address = Object.keys(maybe_wallet_info.result.address.current)[0];
        set_pv_address(the_address);
    }
    else
    {
        h4.innerHTML = "error";
        h4.style.color = "crimson";
        pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
    }
}

async function
form_tx
    (logger: sk.Logger)
    : Promise<void>
{
    // @ts-ignore
    let recipient_id = document.getElementById('tx-info-target')!.value;
    // @ts-ignore
    let amount       = parseInt(document.getElementById('tx-info-amount')!.value);
    let fee          = ae_node.MIN_FEE;
    let sender_id    = pv_address;
    let payload      = 'hainana';
    let spendtx      = {recipient_id : recipient_id,
                        amount       : amount,
                        fee          : fee,
                        sender_id    : sender_id,
                        payload      : payload};
    document.getElementById('spendtx-json')!.innerHTML =
        JSON.stringify(spendtx, undefined, 4);
    let tx = await ae_node.PostSpend(ae_node.URL_TESTNET, spendtx);
    // @ts-ignore
    set_pv_spendtx_base64(tx.tx);
    //document.getElementById('spendtx-base64')!.innerHTML =
    //    JSON.stringify(tx, undefined, 4);
    //
}

async function
sign_tx
    (logger: sk.Logger)
    : Promise<void>
{
    let sign_params = {
        tx: pv_spendtx_base64,
        // returnsigned: false tells it to propagate the transaction
        // returnsigned: true just signs it and returns the signed tx back
        returnSigned: true as true,
        networkId: "ae_uat"
    };
    let signedTx = await sk.tx_sign_noprop('sk-tx-sign-1', sign_params, sk.TIMEOUT_DEF_TX_SIGN_NOPROP_MS, 'sign transaction timed out', logger);
    document.getElementById("sign-spendtx-result")!.innerHTML = JSON.stringify(signedTx, undefined, 4);
}

async function
sign_msg
    (logger: sk.Logger)
    : Promise<void>
{
    let acc_pubkey : string = pv_address;
    // @ts-ignore value property exists because i say so
    let msg_text   : string = document.getElementById('message-text').value;
    let signed_msg = await sk.msg_sign('sk-msg-sign-1', acc_pubkey, msg_text, sk.TIMEOUT_DEF_MSG_SIGN_MS, 'message signing took too long', logger);
    console.log('signed message:', signed_msg);
}

function
main
    ()
{
    // want to create a logger down here
    let logger = sk.cl();
    // custom logger
    window.addEventListener('message', function(evt) { if("to_waellet" === evt.data.type) {console.log(evt)} });
    // the !s are typescript jizz to turn off the warning that the return value
    // of getElementById might be null
    document.getElementById('detect')!.onclick = function() { detect(logger); } ;
    document.getElementById('connect')!.onclick = function() { connect(logger); };
    document.getElementById('address')!.onclick = function() { address(logger); };
    document.getElementById('mk-spendtx')!.onclick = function() { form_tx(logger); };
    document.getElementById('sign-spendtx')!.onclick = function() { sign_tx(logger); };
    document.getElementById('sign-message')!.onclick = function() { sign_msg(logger); };

    //set_pv_address("ak_2XhCkjzTwcq1coXSSzHJoMZkUzTwnjH88zmPGkkowUsFNTo9UE");
}

main();
