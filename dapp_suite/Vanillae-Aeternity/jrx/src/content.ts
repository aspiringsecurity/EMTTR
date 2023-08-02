/**
 * JR Content Script
 *
 * @module
 */


jr_main();


/**
 * This is the "meat" of what is spammed to the page script when the user makes
 * the wallet detectable.
 *
 * If you go read the AWCP documentation, there's a bunch of layers. This is I
 * think the innermost layer.
 */
function detect_msg() {
    return {id        : "jr",
            name      : "JR",
            networkId : "ae_uat",
            origin    : "foobar",
            type      : "extension"};
}




/**
 * Post {@link detect_awcp_msg} into the window message queue
 */
function
post_detect_msg
    (msg: object)
{
    window.postMessage(msg, '*');
}



/**
 * For 2 minutes, make the wallet detectable to a page script
 */
async function
mk_detectable
    (msg: object)
{
    while (true)
    // 3 seconds times 40 is 2 minutes
    // for (let i=1; i<=40; i++)
    {
        console.error('pee');
        post_detect_msg(msg);
        await sleep(3000);
    }
}



/**
 * sleep for the given number of ms
 */
async function
sleep
    (ms: number)
    : Promise<void>
{
    return new Promise(resolve => setTimeout(resolve, ms));
}


/**
 * This handles messages from *other parts of the extension*
 */
function
handler
    (msg: any)
{
    console.error('message: ', msg);
    switch(msg)
    {
        case 'mk-detectable':
            mk_detectable();
            return;
        default:
            console.error('your mom is dead');
    }
}



/**
 * This handles messages from page scripts
 */
function
window_message_handler
    (msg: {data: any})
{
    console.error('the science is coming', msg);
    // example science:
    // {
    //   "type": "to_waellet",
    //   "data": {
    //     "jsonrpc": "2.0",
    //     "id": "ske-connect-1",
    //     "method": "connection.open",
    //     "params": {
    //       "name": "sidekick examples",
    //       "version": 1
    //     }
    //   }
    // }
    let the_science = msg.data;
    console.error('THE SCIENCE: ', the_science);
    console.error('is the science good or bad?');
    if (the_science.type === 'to_waellet')
    {
        console.error('the science is good');
        the_science_is_good(the_science.data);
    }
    else
        console.error('the science is bad');
}



/**
 * window_message_handler handles all messages sent into the event bus,
 * including messages that we sent (yay js). window_message_handler branches on
 * whether the message is for us or not (is the science good or bad?). If the
 * message is for us (the science is good), then this function is triggered.
 *
 * The bottom line is this is the function that *actually* handles messages
 * from the window
 */
function
the_science_is_good
    (awcp_rcp_data: {id     : string | number,
                     method : string,
                     params : object})
{
    // example science data:
    // // layer 3: json rpc
    // {jsonrpc : "2.0",
    //  id      : "ske-connect-1",
    //  method  : "connection.open",
    //            // layer 4: AWCP-specific semantics
    //  params  : {name    : "sidekick examples",
    //             version : 1}}}
    // can assume it is a call
    let msg_ident  = awcp_rcp_data.id;
    let msg_method = awcp_rcp_data.method;
    // branch here on the "method" field
    switch (msg_method)
    {
        case "connection.open":
            post_connect_msg(msg_ident);
            break;
        case "address.subscribe":
            bg_address_subscribe(msg_ident);
            break;
        case "transaction.sign":
            bg_tx_sign(msg_ident, awcp_rcp_data.params);
            break;
        case "message.sign":
            bg_msg_sign(msg_ident, awcp_rcp_data.params);
            break;
        default:
            console.error('the science is worse than i initially thought');
            console.error("we're going to have to put the science to sleep");
    }
}

/**
 * Called in response to "connection.open" requests from page scripts
 *
 * FIXME: this should query the user to see if he wants to connect
 */
function
post_connect_msg
    (msg_ident : string | number)
{
    // : EventData_W2A_connection_open
    // http://localhost:6969/local-awcp-0.2.1/types/EventData_W2A_connection_open.html
    let connect_response =
            {type: "to_aepp",
             data: {jsonrpc: "2.0",
                    id: msg_ident,
                    method: "connection.open",
                    result: detect_msg()}};
    window.postMessage(connect_response, '*');
}



/**
 * Called in response to "address.subscribe" requests from page scripts
 *
 * this is to get the wallet's address
 */
function
bg_address_subscribe
    (msg_ident: string | number)
{
    
}



/**
 * Called in response to "transaction.sign" requests from page scripts
 *
 * user wants us to sign a transaction
 */
function
bg_tx_sign
    (msg_ident : string | number,
     params    : object)
{
}



/**
 * Called in response to "message.sign" requests from page scripts
 *
 * user wants us to sign a message
 */
function
bg_msg_sign
    (msg_ident : string | number,
     params    : object)
{
}


async function
jr_main
    ()
{
    // @ts-ignore browser
   // browser.runtime.onMessage.addListener(handler);
    window.addEventListener('message', window_message_handler);




    /**
     * This is the message that we spam the page script with when the user clicks
     * the "make wallet detectable" button.
     *
     * Contains {@link detect_msg} as a field
     */
    let detect_awcp_msg = {type : "to_aepp",
                           data : {jsonrpc : "2.0",
                                   method  : "connection.announcePresence",
                                   params  : detect_msg()}};

    // mk detectable
    mk_detectable(detect_awcp_msg);
}
