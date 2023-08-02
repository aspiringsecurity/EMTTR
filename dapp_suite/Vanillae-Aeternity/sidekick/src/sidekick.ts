/**
 * # tl;dr
 *
 * 1. {@link detect} the wallet
 * 2. {@link connect} to the wallet
 * 3. Get the wallet's {@link address}
 *
 * From there you can do one of two things
 *
 * 1. Have the wallet sign transactions ({@link tx_sign_noprop})
 * 2. Have the wallet sign arbitrary messages ({@link msg_sign})
 *
 * Forming the transactions and propagating them into the network is your
 * problem.
 *
 * You need a {@link Logger} for most calls. Probably you want {@link cl}. You
 * can write your own if you want but why would you complicate your life like
 * that.
 *
 * @module
 */

// TODONE: add standardized logging interface
// TODONE: logging hooks
// TODONE: invoice
// TODONE: get connect done
// TODONE: make the message queue for responses a map, fill the message queue properly
// TODONE: get it working with superhero
// TODO: jrx

// like: console, http, etc
// allow someone to pass a logger


//-----------------------------------------------------------------------------
// EXPORTS
//-----------------------------------------------------------------------------

export {
    // debugging
        hello,
    // computational dental dams
        Safe,
        unsafe,
        ok,
        error,
        Ok,
        Error,
    // timeout errors
        ERROR_CODE_SkTimeoutError,
        SkTimeoutError,
        sk_timeout,
    // logging bullshit
        Logger,
        WebScale,
        wsl,
        ConsoleLogger,
        cl,
        HttpLogger,
        http_log,
        SeqLogger,
        logger_foreach,
    // timeouts
        // time constants
        MS,
        SEC,
        MIN,
        HR,
        // timeouts
        TIMEOUT_DEF_DETECT_MS,
        TIMEOUT_DEF_CONNECT_MS,
        TIMEOUT_DEF_ADDRESS_MS,
        TIMEOUT_DEF_TX_SIGN_NOPROP_MS,
        TIMEOUT_DEF_MSG_SIGN_MS,
    // API
        // detect
        detect,
        CAPListener,
        // connect
        connect,
        address,
        tx_sign_noprop,
        msg_sign,

    // internals
        sleep,
        bulrtot
};


//-----------------------------------------------------------------------------
// IMPORTS
//-----------------------------------------------------------------------------

import * as awcp from './jex_include/local-awcp-0.2.1/dist/awcp.js';


//-----------------------------------------------------------------------------
// API
//-----------------------------------------------------------------------------

/**
 * Just `console.log`s `hello`. Use for debugging/to check if import worked correctly
 */
function
hello
    ()
    : void
{
    console.log('hello');
}



/**
 * The idea behind this type is that some errors are known to be likely (e.g.
 * the user rejects a transaction request, or something times out, etc).  These
 * errors (called "positive errors") should not generate exceptions. Exceptions
 * should occur when exceptional behavior occurs (e.g. hardware faults,
 * dividing by zero).  Exceptions should not occur on events that are known to
 * be likely.  Instead, branching is the correct idiom.
 *
 * Furthermore, there are often many possible sources of errors.  What this
 * provides is a single "did it work or not" branch point.
 *
 * Further, in practice, all errors (either {@link SkTimeoutError} or {@link
 * awcp.RpcError}) have a field called `code` which uniquely identifies the
 * error, so it's easy to algorithmically respond to specific positive errors.
 * For instance, if the user rejects a request in a popup, that is `code: 4`
 * (see {@link awcp.ERROR_CODE_RpcRejectedByUserError}).  If the user does not
 * do anything within the timeout parameter you specify, that generates a
 * {@link SkTimeoutError} which has `code: 420`.
 *
 * There are two branches: {@link Ok} and {@link Error}
 *
 * ```ts
 * type Ok<ok_t>
 *     = {ok     : true,
 *        result : ok_t};
 *
 * type Error<err_t>
 *     = {ok    : false,
 *        error : err_t};
 *
 * type Safe<ok_t, err_t>
 *     = Ok<ok_t>
 *     | Error<err_t>;
 * ```
 *
 * @example
 * Suppose you are trying to get the user's {@link address}. This pops up a
 * confirmation dialog asking the user if he wants to connect to your
 * application.  There's a good chance the user says no.  There's also a
 * possibility he just doesn't do anything and things just time out.
 *
 * ```ts
 * // there is a button in the document that when pressed triggers this function
 * async function address(logger: sk.Logger): Promise<void>
 * {
 *     let h4 = document.getElementById("addressed")!;
 *     let pre = document.getElementById("address-info")!;
 *
 *     h4.innerHTML = 'addressing...';
 *     h4.style.color = 'GoldenRod';
 *
 *     // try to address to the wallet
 *     // will fail on timeout error
 *     let maybe_wallet_info = await sk.address(
 *             'ske-address-1',
 *             {type: 'subscribe',
 *              value: 'connected'},
 *             sk.TIMEOUT_DEF_ADDRESS_MS,
 *             "failed to address to wallet",
 *             logger
 *         );
 *
 *     console.log(maybe_wallet_info);
 *
 *     // ok means wallet was addressed
 *     if (maybe_wallet_info.ok)
 *     {
 *         h4.innerHTML   = "addressed";
 *         h4.style.color = "green";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
 *         // update global variable
 *         let the_address = Object.keys(maybe_wallet_info.result.address.current)[0];
 *         set_pv_address(the_address);
 *     }
 *     else
 *     {
 *         h4.innerHTML = "error";
 *         h4.style.color = "crimson";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
 *     }
 * }
 * ```
 *
 * The important part is the `if/else`.  That branch point corresponds to "did
 * it work or not?"  The `else` branch corresponds to the subset of "not" that
 * you know is likely: timeouts, rejections, etc.
 */
type Safe<ok_t, err_t>
    = Ok<ok_t>
    | Error<err_t>;



/**
 * See {@link Safe} for context
 *
 * @example
 * ```json
 * {
 *     "ok": true,
 *     "result": {
 *         "type": "to_aepp",
 *         "data": {
 *             "jsonrpc": "2.0",
 *             "id": "sk-msg-sign-1",
 *             "method": "message.sign",
 *             "result": {
 *                 "signature": "3ec195484965a60dd4179fbb616947a85e4e9961cb468816a2a22870382954bc374f8569e4ddc729dfc1b14e09e670b7d2c70c33c592caca29ff6c212f1f8b0f"
 *             }
 *         }
 *     }
 * }
 * ```
 */
type Ok<ok_t>
    = {ok     : true,
       result : ok_t};



/**
 * See {@link Safe} for context
 *
 * @example
 * ```json
 * {
 *     "ok": false,
 *     "error": {
 *         "code": 4,
 *         "data": {},
 *         "message": "Operation rejected by user"
 *     }
 * }
 * ```
 */
type Error<err_t>
    = {ok    : false,
       error : err_t};



/**
 * Constructs an `Ok` value from a pure value
 */
function
ok
    <ok_t>
    (x : ok_t)
    : Ok<ok_t>
{
    return {ok: true, result: x};
}



/**
 * Constructs an `Error` value from a pure value
 */
function
error
    <err_t>
    (x: err_t)
    : Error<err_t>
{
    return {ok: false, error: x};
}



/**
 * Takes a `Safe` value, if `ok`, returns the `ok_t`, or if an error throws the
 * `err_t`
 */
function
unsafe
    <ok_t, err_t>
    (x: Safe<ok_t, err_t>)
    : ok_t
{
    if (x.ok)
        return x.result;
    else
        throw x.error;
}



/** Error code for `SkTimeoutError`s */
const ERROR_CODE_SkTimeoutError = 420;


/**
 * Timeout Error
 */
type SkTimeoutError
    = {code    : 420,
       message : string,
       data    : object};



/**
 * Construct a `SkTimeoutError`
 */
function
sk_timeout
    (message : string,
     data    : object)
{
    return {code    : 420 as 420,   // typescript is great
            message : message,
            data    : data};
}

/**
 * It's web scale
 */
function wsl() { return new WebScale(); }

/** construct a `ConsoleLogger` */
function cl() { return new ConsoleLogger(); }


/**
 * Callbacks you need to implement if you want logging
 */
interface Logger
{
    debug   : (message : string, data : object) => Promise<void>;
    info    : (message : string, data : object) => Promise<void>;
    warning : (message : string, data : object) => Promise<void>;
    error   : (message : string, data : object) => Promise<void>;
}

/**
 * Web scale
 */
class WebScale implements Logger
{
    async debug   (_msg: string, _data: object) { return; }
    async info    (_msg: string, _data: object) { return; }
    async warning (_msg: string, _data: object) { return; }
    async error   (_msg: string, _data: object) { return; }
}

/**
 * does console.log
 */
class ConsoleLogger implements Logger
{
    listener : (e : Event) => void;
    constructor() {
        let this_ptr = this;
        this.listener =
            function (e : Event) {
                // for typescript
                if (e instanceof MessageEvent) {
                    this_ptr.debug(`ConsoleLogger received MessageEvent`, e.data);
                }
            };
    }
    async debug   (_msg: string, _data: object) { console.debug (_msg, _data); }
    async info    (_msg: string, _data: object) { console.log   (_msg, _data); }
    async warning (_msg: string, _data: object) { console.warn  (_msg, _data); }
    async error   (_msg: string, _data: object) { console.error (_msg, _data); }
    listen() { window.addEventListener('message', this.listener); }
    ignore() { window.removeEventListener('message', this.listener); }
}

/**
 * posts request to an HTTP server
 */
class HttpLogger implements Logger
{
    post_endpoint: string;
    listener : (e : Event) => void;
    constructor (post_endpoint: string) {
        this.post_endpoint = post_endpoint;
        let this_ptr = this;
        this.listener =
            function (e : Event) {
                // for typescript
                if (e instanceof MessageEvent) {
                    this_ptr.debug(`HttpLogger received MessageEvent`, e.data);
                }
            };
    }
    async debug   (_msg: string, _data: object) { http_log(this.post_endpoint, 'debug', _msg, _data); }
    async info    (_msg: string, _data: object) { http_log(this.post_endpoint, 'info', _msg, _data); }
    async warning (_msg: string, _data: object) { http_log(this.post_endpoint, 'warning', _msg, _data); }
    async error   (_msg: string, _data: object) { http_log(this.post_endpoint, 'error', _msg, _data); }
    listen() { window.addEventListener('message', this.listener); }
    ignore() { window.removeEventListener('message', this.listener); }
}

async function
http_log
    (post_endpoint  : string,
     log_level      : 'debug' | 'info' | 'warning' | 'error',
     message        : string,
     data           : object)
    : Promise<void>
{
    try
    {
        await fetch(post_endpoint, {method : 'POST',
                                    body   : JSON.stringify({level: log_level,
                                                             message: message,
                                                             data: data},
                                                            undefined,
                                                            4)});
    }
    catch (e)
    {
        console.error(e);
    }
}

/**
 * have an array of loggers fire sequentially
 */
class SeqLogger implements Logger
{
    loggers: Array<Logger>
    constructor (loggers : Array<Logger>) { this.loggers = loggers; }
    async debug   (_msg: string, _data: object) { logger_foreach(this.loggers, 'debug'  , _msg, _data); }
    async info    (_msg: string, _data: object) { logger_foreach(this.loggers, 'info'   , _msg, _data); }
    async warning (_msg: string, _data: object) { logger_foreach(this.loggers, 'warning', _msg, _data); }
    async error   (_msg: string, _data: object) { logger_foreach(this.loggers, 'error'  , _msg, _data); }
}

async function
logger_foreach
    (loggers   : Array<Logger>,
     log_level : 'debug' | 'info' | 'warning' | 'error',
     message   : string,
     data      : object)
    : Promise<void>
{
    for (let logger of loggers)
    {
        switch(log_level)
        {
            case 'debug':
                logger.debug(message, data);
                break;
            case 'info':
                logger.info(message, data);
                break;
            case 'warning':
                logger.warning(message, data);
                break;
            case 'error':
                logger.error(message, data);
                break;
        }
    }
}

//-----------------------------------------------------------------------------
// ACTUAL API
//-----------------------------------------------------------------------------

// UNITS OF TIME

/** Unit of time; `const MS = 1` */
const MS  = 1;
/** `const SEC = 1000*MS` */
const SEC = 1000*MS;
/** `const MIN = 1000*MS` */
const MIN = 60*SEC;
/** `const HR = 60*MIN` */
const HR = 60*MIN;

// TIMEOUTS

/**
 * 7 seconds. Superhero announces itself every 3 seconds, so this is 2
 * announcements + 1 second.
 */
const TIMEOUT_DEF_DETECT_MS  = 7*SEC;

/**
 * 1 second (instaneous in practice)
 */
const TIMEOUT_DEF_CONNECT_MS = 1*SEC;


/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is where you as the developer
 * need to exercise some discretion.
 */
const TIMEOUT_DEF_ADDRESS_MS = 5*MIN;


/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is an instance where you as
 * the developer need to exercise some discretion.
 */
const TIMEOUT_DEF_TX_SIGN_NOPROP_MS = 5*MIN;


/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is an instance where you as
 * the developer need to exercise some discretion.
 */
const TIMEOUT_DEF_MSG_SIGN_MS = 5*MIN;


// Ah ok
//
// so for connection.announcePresence, we just listen and recieve, unwrap, send back
//
// for everything else, we send a request, await a response
//
// either: SkTimeoutError or an RpcError



//-----------------------------------------------------------------------------
// API: detection
//-----------------------------------------------------------------------------

/**
 * This is the first step in connecting to the wallet. Before doing anything
 * else, you have to wait for the wallet to announce itself.
 *
 * This function waits for the wallet to announce itself
 *
 * Wait for wallet to announce itself, and then return.
 *
 * @example
 * ```ts
 * // example call
 * await sk.detect(sk.TIMEOUT_DEF_DETECT_MS,    // timeout
 *                 "failed to detect wallet",   // error on timeout
 *                 sk.cl());                    // cl = console logger
 * // example return data (positive case)
 * {ok     : true,
 *  result : {id        : "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
 *            name      : "Superhero",
 *            networkId : "ae_uat",
 *            origin    : "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
 *            type      : "extension"}}
 * // example return data (negative case)
 * {ok     : false,
 *  error  : {code    : 420,
 *            message : "failed to detect wallet",
 *            data    : {}}}
 * ```
 *
 * @example
 * ```ts
 * // example calling code
 * // from: https://github.com/aeternity/Vanillae/blob/0e030cb39554e9f09e4460d1da4e7654fe7456de/sidekick/examples/src/connection.ts#L34-L66
 * // document has a button with id=detect
 * // this is the function that is triggered when the button is clicked
 * async function detect(logger: sk.Logger): Promise<void> {
 *     let h4 = document.getElementById("detected")!;
 *     let pre = document.getElementById("detect-info")!;
 *
 *     h4.innerHTML = 'detecting...';
 *     h4.style.color = 'GoldenRod';
 *
 *     // try to detect the wallet
 *     // will fail on timeout error
 *     // console logger
 *     let maybe_wallet_info =//: sk.Safe<awcp.EventData_W2A_connection_announcePresence, sk.TimeoutError> =
 *             await sk.detect(sk.TIMEOUT_DEF_DETECT_MS, "failed to detect wallet", logger);
 *
 *     console.log(maybe_wallet_info);
 *
 *     // ok means wallet was detected
 *     if (maybe_wallet_info.ok) {
 *         h4.innerHTML   = "detected";
 *         h4.style.color = "green";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
 *     }
 *     else {
 *         h4.innerHTML = "error";
 *         h4.style.color = "crimson";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
 *     }
 * }
 *
 * // want to create a logger down here
 * let logger = sk.cl();
 * // detect button. The ! turns off a typescript warning, does not change the
 * // code behavior
 * document.getElementById('detect')!.onclick = function() { detect(logger); } ;
 * ```
 */
async function
detect
    (timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Params_W2A_connection_announcePresence, SkTimeoutError>>
{
    let call_params = {timeout_ms   : timeout_ms,
                       timeout_msg  : timeout_msg};
    logger.debug('detect', call_params);
    let listener = new CAPListener(logger);
    logger.debug('detect: listening on window', {});
    listener.listen();
    let result = await listener.raseev(timeout_ms, timeout_msg);
    logger.debug('detect: result', {result:result});
    logger.debug('detect: ignoring window', {});
    listener.ignore();
    return result;
}



/**
 * Listens for `connection.announcePresence` messages
 *
 * @internal
 */
class CAPListener
{
    logger    : Logger;
    listener  : ((e : Event) => void );
    cap_queue : null | awcp.Params_W2A_connection_announcePresence = null;

    constructor(logger: Logger) {
        logger.debug('CAPListener.constructor', {});
        this.logger = logger;
        // js pointer hack
        const this_ptr = this;
        this.listener = function (event : Event) { this_ptr.handle(event); };
    }


    listen() : void {
        this.logger.info('CAPListener.listen', {});
        window.addEventListener('message', this.listener);
    }


    ignore() : void {
        this.logger.info('CAPListener.ignore', {});
        window.removeEventListener('message', this.listener);
    }


    handle(evt : Event) : void {
        this.logger.debug('CAPListener.handle', {event: evt});
        if (evt instanceof MessageEvent)
            this.really_handle(evt);
    }

    really_handle
        (evt : MessageEvent<any>)
        : void
    {
        this.logger.debug('CAPListener.really_handle', {event: evt});
        // Example message data:
        //
        // ```json
        // {
        //     "type": "to_aepp",
        //     "data": {
        //         "jsonrpc": "2.0",
        //         "method": "connection.announcePresence",
        //         "params": {
        //             "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
        //             "name": "Superhero",
        //             "networkId": "ae_mainnet",
        //             "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
        //             "type": "extension"
        //         }
        //     }
        // }
        // ```
        //
        // Example queue data:
        //
        // ```json
        // {
        //     "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
        //     "name": "Superhero",
        //     "networkId": "ae_mainnet",
        //     "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
        //     "type": "extension"
        // }
        // ```
        let queue_empty   : boolean = !this.cap_queue;
        let msg_is_for_us : boolean = evt.data.type === "to_aepp";
        let is_cap_msg    : boolean = evt.data.data.method === "connection.announcePresence";
        let we_rollin     : boolean = queue_empty && msg_is_for_us && is_cap_msg;

        this.logger.debug('CAPListener.really_handle: branching variables regarding how to handle this event',
                          {queue_empty   : queue_empty,
                           msg_is_for_us : msg_is_for_us,
                           is_cap_msg    : is_cap_msg,
                           we_rollin     : we_rollin,
                           event         : evt});
        if (we_rollin) {
            this.logger.debug('CAPListener.really_handle: queue empty, message is for us, and it is the message we want, so adding to queue',
                              {event: evt,
                               new_queue: evt.data.data.params});
            this.cap_queue = evt.data.data.params;
        }
        else {
            this.logger.debug("CAPListener.really_handle: for whatever reason, we're ignoring this event",
                              {event: evt});
        }
    }


    async raseev
        (timeout_ms  : number,
         timeout_msg : string)
        : Promise<Safe<awcp.Params_W2A_connection_announcePresence, SkTimeoutError>>
    {
        this.logger.debug('CAPListener.raseev',
                           {timeout_ms  : timeout_ms,
                            timeout_msg : timeout_msg});
        // stupid js pointer hack
        let this_ptr = this;
        let lambda_that_must_return_true_to_unblock =
                function () {
                    // this means queue is not empty
                    return !!(this_ptr.cap_queue);
                };

        let get_result =
                function () {
                    this_ptr.logger.debug('CAPListener.raseev.get_result', {});
                    return this_ptr.cap_queue as awcp.Params_W2A_connection_announcePresence;
                };
        let result =
                await bulrtot<awcp.Params_W2A_connection_announcePresence>
                             (lambda_that_must_return_true_to_unblock,
                              get_result,
                              timeout_ms,
                              timeout_msg,
                              this.logger);
        return result;
    }
}



//-----------------------------------------------------------------------------
// API: connection
//-----------------------------------------------------------------------------



/**
 * Connect to the wallet. This is a necessary step if you want {@link address}
 * to work. You **must** wait for Superhero to announce itself (see {@link
 * detect}) before attempting to connect. This has the same return data as
 * {@link detect}. This does not pop up the confirmation dialog for the user.
 * Superhero never rejects the connection request. This call is instantaneous
 * in practice.
 *
 * @example
 * ```ts
 * // example calling code
 * await sk.connect('ske-connect-1',                // message id, can be arbitrary string/number
 *                  {name: 'sidekick examples',     // name can be any string
 *                   version: 1},                   // version must be 1
 *                                                  // optional field networkId which is ae_uat or ae_mainnet
 *                  sk.TIMEOUT_DEF_CONNECT_MS,      // timeout
 *                  "failed to connect to wallet",  // timeout error message
 *                  sk.cl());                       // logger
 * // example return data (positive case)
 * {ok     : true,
 *  result : {id        : "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
 *            name      : "Superhero",
 *            networkId : "ae_uat",
 *            origin    : "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
 *            type      : "extension"}}
 * // example return data (negative case)
 * {ok     : false,
 *  error  : {code    : 420,
 *            message : "failed to detect wallet",
 *            data    : {}}}
 * ```
 *
 * @example
 * ```ts
 * // example calling code
 * // from: https://github.com/aeternity/Vanillae/blob/0e030cb39554e9f09e4460d1da4e7654fe7456de/sidekick/examples/src/connection.ts#L68-L109
 * // document has a button with id=connect
 * // this is the function that is triggered when the button is clicked
 * // you want your user to do the detect sequence first
 * async function connect (logger: sk.Logger) : Promise<void> {
 *     let h4 = document.getElementById("connected")!;
 *     let pre = document.getElementById("connect-info")!;
 *
 *     h4.innerHTML = 'connecting...';
 *     h4.style.color = 'GoldenRod';
 *
 *     // try to connect to the wallet
 *     // will fail on timeout error
 *     let maybe_wallet_info = await sk.connect(
 *             'ske-connect-1',
 *             {name: 'sidekick examples',
 *              version: 1},
 *             sk.TIMEOUT_DEF_CONNECT_MS,
 *             "failed to connect to wallet",
 *             logger
 *         );
 *
 *     console.log(maybe_wallet_info);
 *
 *     // ok means wallet was connected
 *     if (maybe_wallet_info.ok)
 *     {
 *         h4.innerHTML   = "connected";
 *         h4.style.color = "green";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
 *     }
 *     else
 *     {
 *         h4.innerHTML = "error";
 *         h4.style.color = "crimson";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
 *     }
 * }
 *
 * let logger = sk.cl();
 * document.getElementById('connect')!.onclick = function() { connect(logger); };
 * ```
 */
async function
connect
    (id          : number | string,
     params      : awcp.Params_A2W_connection_open,
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Result_W2A_connection_open, awcp.RpcError | SkTimeoutError>>
{
    logger.debug('connect', {id:id, params:params, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result =
        await msgr.send_raseev
                <"connection.open", awcp.Params_A2W_connection_open, awcp.Result_W2A_connection_open>
                (id, "connection.open", params, timeout_ms, timeout_msg);
    return result;
}



//-----------------------------------------------------------------------------
// API: get address
//-----------------------------------------------------------------------------


/**
 * Get the user's address.
 *
 * @example
 * ```ts
 * // example positive return
 * {ok     : true,
 *  result : {subscription : ["connected"],
 *            address      : {current   : {"ak_2XhCkjzTwcq1coXSSzHJoMZkUzTwnjH88zmPGkkowUsFNTo9UE": {}},
 *                            connected : {"ak_21HW2BeR8KQnzB76b9RSeNAXFf8SEvquLG3ichyLaXhdxUpXe9" : {},
 *                                         "ak_Bd9rA8pDWucwfriVp6Zgb68csxanCzWDqstyoBKBbzUnNhpKQ"  : {},
 *                                         "ak_TuwioiZCt3Ajx9dgVS9qdnS9VW1t4GMWFML5zBPgzouZUGUDA"  : {},
 *                                         "ak_ywR1N7GDpj7djeEEEnHSTmYbQmxvWCvFgfsLpxdFpK1ptkZMU"  : {}}}}}
 * // example negative return
 * {ok    : false,
 *  error : {code    : 4,
 *           data    : {},
 *           message : "Operation rejected by user"}}
 * ```
 */
async function
address
    (id          : number | string,
     params      : awcp.Params_A2W_address_subscribe,
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Result_W2A_address_subscribe, awcp.RpcError | SkTimeoutError>>
{
    logger.debug('address', {id:id, params:params, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result =
        await msgr.send_raseev
                <"address.subscribe", awcp.Params_A2W_address_subscribe, awcp.Result_W2A_address_subscribe>
                (id, "address.subscribe", params, timeout_ms, timeout_msg);
    return result;
}



//-----------------------------------------------------------------------------
// API: message sign
//-----------------------------------------------------------------------------


/**
 * Sign the given message, return the **HASHED AND SALTED** message signature
 * (see IMPORTANT CAVEAT section), encoded in hexadecimal
 *
 * ## IMPORTANT CAVEAT: HASHING AND SALTING
 *
 * In order to exclude the possibility of someone using this functionality to
 * trick the user into signing a transaction, the wallet salts and hashes the
 * message, and *then* signs the salted/hashed message.
 *
 * Therefore, naively attempting to verify the signature will not work. You
 * must apply the same preprocessing steps as the wallet, **THEN** check the
 * signature against the salted/hashed message.
 *
 * ```erlang
 * -spec hashed_salted_msg(Message) -> HashedSaltedMessage
 *     when Message             :: binary(),
 *          HashedSaltedMessage :: binary().
 * % @doc Salt the message then hash with blake2b. See:
 * % 1. https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L83-L85
 * % 2. https://github.com/aeternity/eblake2/blob/60a079f00d72d1bfcc25de8e6996d28f912db3fd/src/eblake2.erl#L23-L25
 *
 * hashed_salted_msg(Msg) ->
 *     {ok, HSMsg} = eblake2:blake2b(32, salted_msg(Msg)),
 *     HSMsg.
 *
 *
 *
 * -spec salted_msg(Message) -> SaltedMessage
 *     when Message       :: binary(),
 *          SaltedMessage :: binary().
 * % @doc Salt the message the way Superhero does before signing.
 * %
 * % See: https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L171-L175
 *
 * salted_msg(Msg) when is_binary(Msg) ->
 *     P = <<"aeternity Signed Message:\n">>,
 *     {ok, SP}   = btc_varuint_encode(byte_size(P)),
 *     {ok, SMsg} = btc_varuint_encode(byte_size(Msg)),
 *     <<SP/binary,
 *       P/binary,
 *       SMsg/binary,
 *       Msg/binary>>.
 *
 *
 *
 * -spec btc_varuint_encode(Integer) -> Result
 *     when Integer :: integer(),
 *          Result  :: {ok, Encoded :: binary()}
 *                   | {error, Reason :: term()}.
 * % @doc Bitcoin varuint encode
 * %
 * % See: https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
 *
 * btc_varuint_encode(N) when N < 0 ->
 *     {error, {negative_N, N}};
 * btc_varuint_encode(N) when N < 16#FD ->
 *     {ok, <<N>>};
 * btc_varuint_encode(N) when N =< 16#FFFF ->
 *     NBytes = eu(N, 2),
 *     {ok, <<16#FD, NBytes/binary>>};
 * btc_varuint_encode(N) when N =< 16#FFFF_FFFF ->
 *     NBytes = eu(N, 4),
 *     {ok, <<16#FE, NBytes/binary>>};
 * btc_varuint_encode(N) when N < (2 bsl 64) ->
 *     NBytes = eu(N, 8),
 *     {ok, <<16#FF, NBytes/binary>>}.
 *
 * % eu = encode unsigned (little endian with a given byte width)
 * % means add zero bytes to the end as needed
 * eu(N, Size) ->
 *     Bytes = binary:encode_unsigned(N, little),
 *     NExtraZeros = Size - byte_size(Bytes),
 *     ExtraZeros = << <<0>> || _ <- lists:seq(1, NExtraZeros) >>,
 *     <<Bytes/binary, ExtraZeros/binary>>.
 * ```
 *
 * @example
 * This function is triggered when a "sign message" button is pressed. A text
 * input with `id="message-text"` contains the message to be signed.
 * ```ts
 * async function sign_msg (logger: sk.Logger) : Promise<void> {
 *     let acc_pubkey : string = pv_address;
 *     // @ts-ignore value property exists because i say so
 *     let msg_text   : string = document.getElementById('message-text').value;
 *     let signed_msg = await sk.msg_sign('sk-msg-sign-1', acc_pubkey, msg_text, sk.TIMEOUT_DEF_MSG_SIGN_MS, 'message signing took too long', logger);
 *     console.log('signed message:', signed_msg);
 * }
 * ```
 */
async function
msg_sign
    (id             : number | string,
     account_pubkey : string,
     message        : string,
     timeout_ms     : number,
     timeout_msg    : string,
     logger         : Logger)
    : Promise<Safe<awcp.Result_W2A_msg_sign, awcp.RpcError | SkTimeoutError>>
{
    logger.debug('msg_sign', {id:id, account_pubkey:account_pubkey, message:message, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result =
        await msgr.send_raseev
                <"message.sign", awcp.Params_A2W_msg_sign, awcp.Result_W2A_msg_sign>
                (id, "message.sign", {onAccount: account_pubkey, message: message}, timeout_ms, timeout_msg);
    return result;
}



//-----------------------------------------------------------------------------
// API: tx sign (no prop)
//-----------------------------------------------------------------------------
async function
tx_sign_noprop
    (id          : number | string,
     params      : awcp.Params_A2W_tx_sign_noprop,
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Result_W2A_tx_sign_noprop, awcp.RpcError | SkTimeoutError>>
{
    logger.debug('address', {id:id, params:params, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result =
        await msgr.send_raseev
                <"transaction.sign", awcp.Params_A2W_tx_sign_noprop, awcp.Result_W2A_tx_sign_noprop>
                (id, "transaction.sign", params, timeout_ms, timeout_msg);
    return result;
}



class MsgR {
    logger    : Logger;
    listener  : ((e : Event) => void );
    // FIXDME: make this a map
    queue     : Map<string|number, object> = new Map();

    constructor (logger : Logger) {
        this.logger = logger;
        this.logger.debug('MsgR.constructor', {});
        const this_ptr = this;
        this.listener = function (event : Event) { this_ptr.handle(event); }
    }
    listen () : void {
        this.logger.debug('MsgR.listen', {});
        window.addEventListener('message', this.listener);
    }
    ignore () : void {
        this.logger.debug('MsgR.ignore', {});
        window.removeEventListener('message', this.listener);
    }
    handle (evt : Event) : void {
        this.logger.debug('MsgR.handle', {event:evt});
        if (evt instanceof MessageEvent)
            this.really_handle(evt);
    }
    really_handle (evt : MessageEvent<any>) {
        this.logger.debug('MsgR.really_handle', {event:evt});
        // message is
        //
        // raseeving based on id
        //
        // {
        //     "type": "to_aepp",
        //     "data": {
        //         "jsonrpc": "2.0",
        //         "method": don't care,
        //         "id": the key
        //     }
        // }
        //
        // value is the entire message data
        // is it for us, and does it have an id field
        if ((evt.data.type === "to_aepp") && !!(evt.data.data.id))
        {
            // now we know it's a message for us
            let key = evt.data.data.id;
            let val = evt.data;
            this.queue.set(key, val);
        }
    }
    async send_raseev
        <method_s extends string,
         params_t extends object,
         result_t extends any>
        (id          : number | string,
         method      : method_s,
         params      : params_t,
         timeout_ms  : number,
         timeout_msg : string)
        : Promise<Safe<result_t, SkTimeoutError | awcp.RpcError>>
    {
        this.logger.debug('MsgR.send_and_raseev',
                          {id          : id,
                           method      : method,
                           params      : params,
                           timeout_ms  : timeout_ms,
                           timeout_msg : timeout_msg});
        // make the message
        let window_msg = mk_window_msg(id, method, params);
        this.logger.debug('MsgR.send_and_raseev posting message', {window_msg: window_msg});
        // listen
        this.listen();
        // send the message
        window.postMessage(window_msg);
        // receive reply
        let response: Safe<awcp.EventData_W2A<awcp.RpcResp<method_s, result_t>>, SkTimeoutError> =
                await this.raseev(id, timeout_ms, timeout_msg);
        // unwrap the rpc jizz
        let result: Safe<result_t, awcp.RpcError | SkTimeoutError> =
                Safe_AWCP_W2A_Msg_to_Safe_result(response);
        // ignore target
        this.ignore();
        return result;
    }
    async raseev
        <result_t extends object>
        (id          : string | number,
         timeout_ms  : number,
         timeout_msg : string)
        : Promise<Safe<result_t, SkTimeoutError>>
    {
        this.logger.debug('MsgQ.raseev', {id:id, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
        // js pointer hack
        let this_ptr = this;
        let lambda_that_must_return_true_to_unblock = function () { return this_ptr.queue.has(id); };
        let result_fun = function () { return (this_ptr.queue.get(id) as result_t); };
        let result = await bulrtot(lambda_that_must_return_true_to_unblock,
                                   result_fun,
                                   timeout_ms,
                                   timeout_msg,
                                   this.logger);
        return result;
    }
}


function
mk_window_msg
    <method_s extends string,
     params_t extends object>
    (id     : number | string,
     method : method_s,
     params : params_t)
    : awcp.EventData_A2W<awcp.RpcCall<method_s, params_t>>
{
    let rpc_message : awcp.RpcCall<method_s, params_t> =mk_rpc_message(id, method, params);
    return {type: "to_waellet",
            data: rpc_message};
}

function
mk_rpc_message
    <method_s extends string,
     params_t extends object>
    (id     : number | string,
     method : method_s,
     params : params_t)
    : awcp.RpcCall<method_s, params_t>
{
    return {jsonrpc : "2.0",
            id      : id,
            method  : method,
            params  : params};
}


///**
// * Returns the value of `dispatchEvent`
// *
// * See https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/dispatchEvent
// */
//function
//send
//    <data_t extends any>
//    (tgt  : EventTarget & MessageEventSource,
//     data : data_t)
//    : boolean
//{
//    // TODO: look at options besides `data`
//    // See: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/MessageEvent
//    let msg_event: MessageEvent<data_t> = new MessageEvent('message', {data: data, source: tgt});
//    let result = tgt.dispatchEvent(msg_event);
//    return result;
//}
//

//-----------------------------------------------------------------------------
// INTERNALS
//-----------------------------------------------------------------------------


/**
 * Stack overflow: https://stackoverflow.com/questions/951021/what-is-the-javascript-version-of-sleep
 *
 * No fucking idea what's going on here
 *
 * Some crazy async hack bullshit
 *
 * It works, who cares
 *
 * @internal
 */
async function
sleep
    (ms : number)
{
    return new Promise(resolve => setTimeout(resolve, ms));
}


/**
 * Block until lambda returns true or timeout
 *
 * `timeout_ms` should be divisible by `50`
 *
 * @internal
 */
async function
bulrtot
    <ok_t>
    (fun         : (() => boolean),
     result_fun  : (() => ok_t),
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<ok_t, SkTimeoutError>>
{
    logger.debug('bulrtot (block until lambda returns true or timeout)',
                 {timeout_ms  : timeout_ms,
                  timeout_msg : timeout_msg});
    let max_iters : number = Math.floor(timeout_ms / 50);
    logger.debug('bulrtot: iterating every 50 milliseconds',
                 {max_iters: max_iters});

    for(let i = 1; i <= max_iters; i++)
    {
        if (fun())
        {
            logger.debug('bulrtot: lambda returned true on i-th iteration',
                         {i:i, max_iters:max_iters});
            let result = ok(result_fun());
            logger.debug('bulrtot: result (ok)',
                         {result: result});
            return result;
        }
        else
            await sleep(50);
    }

    logger.debug('bulrtot: max iterations exceeded', {max_iters:max_iters});
    let result = sk_timeout(timeout_msg, {});
    logger.debug('bulrtot: result (error)', {result: result});
    return error(result);
}



/**
 * Converts a `Safe`-wrapped `RpcResp` (which may be a success/error) to a
 * `Safe` value
 *
 * Errors can be generated from one of two places: from the wallet (which
 * encodes it in the RPC response), or from sidekick via a timeout.
 *
 * Basically, when we call `bulrtot`, we're going to get back a `Safe`-wrapped
 * `awcp.RpcResp`, which may be success value or an error value.
 *
 * In the timeout error case, preserve.
 *
 * In the case of RPC success, this unwraps whatever is in the `RpcResp`.
 *
 * In the case of RPC failure, this unwraps the error.
 *
 * @internal
 */
function
Safe_AWCP_W2A_Msg_to_Safe_result
    <method_s extends string,
     success_t extends any>
    (safe_w2a_msg : Safe<awcp.EventData_W2A<awcp.RpcResp<method_s, success_t>>, SkTimeoutError>)
    : Safe<success_t,
           awcp.RpcError | SkTimeoutError>
{
    // if input is a success (i.e. NOT a Timeout error), branch on if it's an rpc
    // error
    if (safe_w2a_msg.ok)
    {
        // we have 
        // {ok: true, result: {type: "to_aepp", data: rpc jizz}}
        // want to pull out the rpc jizz
        // case split on whether or not there was an RPC error (i.e. an error generated by the wallet)
        // then our top level return is a safety-wrapped error
        // which is either {ok, TheActualResultWeWant} or {error, RpcError}
        // (this branch of the if) or {error, TimeoutError} (the else branch)
        let ok_w2a_msg = safe_w2a_msg.result;
        let rpc_resp: awcp.RpcResp<method_s, success_t> = ok_w2a_msg.data;

        // From AWCP:
        // /**
        //  * This is the shape of unsuccessful responses
        //  */
        // type RpcResp_error
        //     <method_s extends string>
        //     = {jsonrpc : "2.0",
        //        id      : number | string,
        //        method  : method_s,
        //        error   : RpcError};
        // /**
        //  * This is the shape of successful responses
        //  */
        // type RpcResp_ok
        //     <method_s extends string,
        //      result_t extends any>
        //     = {jsonrpc : "2.0",
        //        id      : number | string,
        //        method  : method_s,
        //        result  : result_t};
        // /**
        //  * This is the shape of generic responses
        //  */
        // type RpcResp
        //     <method_s extends string,
        //      result_t extends any>
        //     = RpcResp_ok<method_s, result_t>
        //     | RpcResp_error<method_s>;

        // so this may be an RpcResp_ok or an RpcResp_error
        // the next line figures that out
        // @ts-ignore typescript is mad because the property that i'm testing to see if it exists might not exist
        let rpc_resp_is_ok: boolean = !!(rpc_resp.result);
        // branch on if we're an RpcResp_ok or an RpcResp_error
        if (rpc_resp_is_ok) {
            // ok so here we're in the RpcResp_ok branch
            // the `result` field exists and we have it
            let the_actual_result: success_t = (rpc_resp as awcp.RpcResp_ok<string, success_t>).result;
            return ok(the_actual_result);
        }
        // error case:
        else {
            let the_error: awcp.RpcError = (rpc_resp as awcp.RpcResp_error<string>).error;
            return error(the_error);
        }
    }
    // this is the timeout error case
    //
    // in which case, the result is what we want
    else
    {
        return safe_w2a_msg;
    }
}
