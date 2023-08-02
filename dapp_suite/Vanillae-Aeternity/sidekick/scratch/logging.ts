

//------------------------------------------------------------------------------
// LOGGERS
//------------------------------------------------------------------------------


/**
 * Standard logging interface
 */
interface Logger
{
    debug   : (message: string, misc: any) => Promise<void>;
    info    : (message: string, misc: any) => Promise<void>;
    warning : (message: string, misc: any) => Promise<void>;
    error   : (message: string, misc: any) => Promise<void>;
}


/**
 * Logger that logs sequentially to a bunch of other loggers
 */
class Loggers implements Logger
{
    loggers : Array<Logger> = [];

    async debug
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        let fun =
                function (logger)
                {
                    logger.debug(msg, misc);
                };
        this.loggers.foreach(fun);
    }


    async info
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        let fun =
                function (logger)
                {
                    logger.info(msg, misc);
                };
        this.loggers.foreach(fun);
    }


    async warning
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        let fun =
                function (logger)
                {
                    logger.warning(msg, misc);
                };
        this.loggers.foreach(fun);
    }


    async error
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        let fun =
                function (logger)
                {
                    logger.error(msg, misc);
                };
        this.loggers.foreach(fun);
    }
}



/**
 * Logs all window messages to the console
 */
class ConsoleLogger implements Logger
{
    listener : ((e: Event) => void);

    constructor
        ()
    {
        // js pointer hack
        const this_ptr = this;
        this.listener =
            function (event : Event)
            {
                this_ptr.handle(event);
            }
    }


    listen
        (tgt : EventTarget)
        : void
    {
        tgt.addEventListener('message', this.listener);
    }


    ignore
        (tgt : EventTarget)
        : void
    {
        tgt.removeEventListener('message', this.listener);
    }


    handle
        (evt : MessageEvent<any>)
        : void
    {
        this.info("handling event", evt);
        return;
    }


    async debug
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        console.debug(msg, misc);
    }


    async info
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        console.info(msg, misc);
    }


    async warning
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        console.warn(msg, misc);
    }


    async error
        (msg  : string,
         misc : any)
        : Promise<void>
    {
        console.error(msg, misc);
    }
}


/**
 * You provide a function that converts any `MessageEvent` to an HTTP request,
 * and this fires the request each time any such event occurs
 */
class HttpLogger
{
    listener    : ((e : Event)             => Promise<void>);
    transformer : ((e : MessageEvent<any>) => Request)

    constructor
        (transformer : (e : MessageEvent<any>) => Request)
    {
        this.transformer = transformer;

        // js pointer hack
        const this_ptr = this;
        this.listener =
            async function (event : Event)
            {
                // @ts-ignore typescript can't figure out that this is guaranteed to be a MessageEvent
                this_ptr.handle(event);
            }
    }


    listen
        (tgt : EventTarget)
        : void
    {
        // @ts-ignore beta type inference
        tgt.addEventListener('message', this.listener);
    }


    ignore
        (tgt : EventTarget)
        : void
    {
        // @ts-ignore beta type inference
        tgt.removeEventListener('message', this.listener);
    }


    async handle
        (evt : MessageEvent<any>)
        : Promise<void>
    {
        let req = this.transformer(evt);
        fetch(req);
        return;
    }
}

