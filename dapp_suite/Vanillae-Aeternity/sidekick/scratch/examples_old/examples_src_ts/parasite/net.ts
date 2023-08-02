//-------------------------------------------------------------------
// Common networking functions
//
// Mozilla fetch docs : https://developer.mozilla.org/en-US/docs/Web/API/fetch
//-------------------------------------------------------------------



/* pf = pretty format
*/
function pf(x : any) : string
{
    return JSON.stringify(x, undefined, 4);
}



//-------------------------------------------------------------------
// FUNCTIONS
//-------------------------------------------------------------------

// GET request with return type of JSON
async function
get_json(url: string)
    : Promise<any>
{
    let response = await fetch(url);
    let ret      = await response.json();
    return ret;
}



// GET request with return type of JSON
async function
get_json_response(url: string)
    : Promise<Response>
{
    let response = await fetch(url);
    //let ret      = await response.json();
    return response;
}



// POST request with content type of json and return type of JSON
async function
post_json(url      : string,
          body_obj : any)
    : Promise<any>
{
    let body_str = pf(body_obj);
    let req_opts = {method  : 'POST',
                    body    : body_str,
                    headers : {"Content-Type": "application/json"}};
    let response = await fetch(url, req_opts);
    let ret      = await response.json();
    return ret;
}


async function
post_json_response(url      : string,
                   body_obj : any)
    : Promise<Response>
{
    let body_str = pf(body_obj);
    let req_opts = {method  : 'POST',
                    body    : body_str,
                    headers : {"Content-Type": "application/json"}};
    let response = await fetch(url, req_opts);
    return response;
}



//-------------------------------------------------------------------
// EXPORTS
//-------------------------------------------------------------------
export {
    get_json,
    get_json_response,
    post_json,
    post_json_response
}
