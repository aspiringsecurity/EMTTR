import * as ae_node  from './parasite/ae_node.js';
import * as sk from '../sidekick_dist/dist_js/sidekick.js';

/**
 * Transfer money to target address
 */
async function
transfer(skl         : sk.Skylight,
         target_addr : string,
         amount      : number)
    : Promise<any>
{
    let endpoint = ae_node.URL_TESTNET;
    let src_addr = await sk.address(skl, sk.TIMEOUT_DEF_ADDRESS);
    let spendtx  = {'recipient_id' : target_addr,
                    'amount'       : amount,
                    'fee'          : ae_node.MIN_FEE,
                    'sender_id'    : src_addr,
                    'payload'      : ""};
    let tx_obj   = await ae_node.PostSpend(endpoint, spendtx) as ae_node.Tx;
    let ret      =
        await sk.tx_sign_no_propagate(skl,
                                      tx_obj,
                                      sk.NETWORK_ID_TESTNET,
                                      sk.TIMEOUT_DEF_SIGN);
    return ret
}


function
current_to_done(class_name : string)
    : void
{
    let class_items = document.getElementsByClassName(class_name);
    for (let class_item of class_items)
    {
        class_item.classList.remove('current');
        class_item.classList.add('done');
    }
}



function
disabled_to_current(class_name : string)
    : void
{
    let class_items = document.getElementsByClassName(class_name);
    for (let class_item of class_items)
    {
        class_item.classList.remove('disabled');
        class_item.classList.add('current');
    }
}



// connect to superhero do what I mean
async function
step1(skl : sk.Skylight)
    : Promise<void>
{
    //// await sk.connect_dwim(skl,
    ////                       sk.TIMEOUT_DEF_DETECT,
    ////                       sk.TIMEOUT_DEF_CONNECT,
    ////                       sk.TIMEOUT_DEF_ADDRESS);

    //console.log('telling to listen!');
    //await sk.listen(skl);
    //console.log('listening!');

    //console.log('detecting!');
    //await sk.detect_dwim(skl, sk.);
    //console.log('detected!');

    //console.log('connecting!');
    //await skl.connect(60000);
    //console.log('connected!');

    //console.log('addressing!');
    //await skl.address(60000);
    //console.log('addressed!');
    ////                       sk.TIMEOUT_DEF_DETECT,
    ////                       sk.TIMEOUT_DEF_CONNECT,
    ////                       sk.TIMEOUT_DEF_ADDRESS);

    // get wallet address
    let wallet_addr = await sk.handshake_def(skl);

    // put it in the thing
    // bang = turn off the null warning
    document.getElementById('user-wallet-address')!.innerHTML = wallet_addr;

    // update style
    current_to_done('step1');
    disabled_to_current('step2');
}


// do tx
async function
step2(skl : sk.Skylight)
    : Promise<void>
{
    // The angle bracket is typescript type inference jizz; doesn't
    // change runtime behavior
    //
    // https://stackoverflow.com/questions/12989741/the-property-value-does-not-exist-on-value-of-type-htmlelement
    let target_addr = (<HTMLInputElement>document.getElementById('step2-recip')).value;
    let amts        = (<HTMLInputElement>document.getElementById('step2-amt')).value;
    let amt         = parseInt(amts);

    // FIXME (dak)
    // @ts-ignore implicit any
    let my_transfer      = await transfer(skl, target_addr, amt);
    let my_transfer_text = sk.pf(my_transfer);

    // add info
    // bang is typescript jizz which means assume not null
    document.getElementById("step3-transaction-info")!.innerHTML =
        my_transfer_text;

    // update style
    current_to_done('step2');
    disabled_to_current('step3');
}


async function main()
{
    //// this turns on the console logger
    //sidekick.snoop_console();

    let skl = await sk.start();

    // as = type inference helper; doesn't affect runtime js
    let step1_fun =
            async function()
            {
                step1(skl)
            }
    let step2_fun =
            async function()
            {
                step2(skl)
            }

    // the exclamation point turns off the "object is possibly null"
    // typecheck
    document.getElementById('btn-step1')!.addEventListener('click', step1_fun);
    document.getElementById('btn-step2')!.addEventListener('click', step2_fun);

    //let foo = async () => {console.log(my_skylight.msgr.msgq.queue);};
    //while(true) {
    //    await foo();
    //    await helpers.sleep(1000);
    //}
}

main();
