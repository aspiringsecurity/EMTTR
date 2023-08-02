/**
 * "Safe" error handling
 *
 * The idea here is that there are situations where it is known
 *
 * 1. a given function call is likely to fail
 * 2. the likely errors can be enumerated
 *
 * ```typescript
 * type Safe<ok_t, err_t>
 *     = Ok<ok_t>
 *     | Error<err_t>;
 *
 * type Ok<ok_t>
 *     = {ok     : true,
 *        result : ok_t};
 *
 * type Error<err_t>
 *     = {ok    : false,
 *        error : err_t};
 * ```
 *
 * These are called "positive errors".  An example would be a page script
 * asking a browser wallet extension to sign a transaction. The following
 * errors, among others, are likely:
 *
 * - the user does not have a wallet installed
 * - the user has a wallet but does not have the correct signing key
 * - the user rejects the transaction
 * - the sign request timed out
 *
 * These errors should not generate exceptions, as these behaviors are to some
 * degree "expected".
 */

export {
    Safe,
    Ok,
    Error,
    ok,
    error,
    unsafe
}


/**
 * Type that catches positive errors
 */
type Safe<ok_t, err_t>
    = Ok<ok_t>
    | Error<err_t>;


/**
 * Ok type
 */
type Ok<ok_t>
    = {ok     : true,
       result : ok_t};


/**
 * Error type
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
