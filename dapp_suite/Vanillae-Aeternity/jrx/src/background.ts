/**
 * JR Controller
 *
 * Note that this is a *thread*.  We put our business and storage logic within
 * this thread.  The reason we do that is we don't want to deal with the
 * problem of conflicting concurrent writes..
 *
 * @module
 */


export type {
    jr_data,
    named_keypair,
    keypair
}

export {
    // top-level getters/setters
};



/**
 * The data structure we store
 */
type jr_data
    = {version        : 0,
       named_keypairs : Array<named_keypair>};



/**
 * This is the default {@link jr_data}.
 *
 * @private
 */
let jr_data_mzero = {version        : 0 as 0,   // yay ts type inference can't infer 0 is of type 0
                     named_keypairs : []};



/**
 * Every keypair has to have a name
 */
type named_keypair
    = {name : string}
      & keypair;



/**
 * From NaCl
 */
type keypair
    = {secretKey : Uint8Array,
       publicKey : Uint8Array};



/**
 * Get the state from the system
 */
async function
fetch_migrate
    ()
    : Promise<jr_data>
{
    // try to get data
    let browser_data = await browser.storage.local.get('jr');
    // test if it's there
    if (!!(browser_data.jr))
        return browser_data.jr;
    // if the data is not there
    else
        return jr_data_mzero;
}



/**
 * Put state
 *
 * @private
 */
async function
put
    (state : jr_data)
    : Promise<void>
{
    await browser.storage.local.set({jr: state});
}
