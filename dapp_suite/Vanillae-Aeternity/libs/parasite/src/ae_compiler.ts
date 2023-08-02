/*******************************************************************
** Compiler API
**
** The manner in which this module is laid out differs meaningfully
** from the `ae_node` module.
**
** - ae_node basically exposes a subset of the node interface as
**   functions. It only black-boxes away the networking aspects.
**
** - part of the reason for this is that the data structures that get
**   sent to the node tend to be pretty involved (lots of fields)
**
** - in general, the node interface is significantly more complicated 
**   than the compiler interface
**
** - by contrast, the data structures that get sent to the compiler
**   tend to be pretty simple and only have a small number of fields
**
** - the manner in which this manifests is that ae_node functions
**   tend to have a weird esoteric data structure as the input,
**   because otherwise there would be too many parameters.
**
**   here, there are few parameters, and they are passed directly
**
** - in general, the compiler has fewer things it can do, and the
**   things it does tend to depend on less information. so this
**   interface is significantly simpler than the node interface
*******************************************************************/

import * as net from './net.js';

const URL_COMPILER = 'https://compiler.aepps.com';
// endpoints
const EPT_CompileContract  = URL_COMPILER + '/compile';
const EPT_EncodeCalldata   = URL_COMPILER + '/encode-calldata';


//-------------------------------------------------------------------
// HELPERS
//-------------------------------------------------------------------

// make a CompileOpts data structure
function
compile_options(filename: string)
    : object
{
    return {"backend"     : "fate",
            "file_system" : {},
            "src_file"    : filename};
}



//-------------------------------------------------------------------
// API CALLS
//-------------------------------------------------------------------

// send back compile response
async function
CompileContract(code     : string,
                filename : string)
    : Promise<Response>
{
    let send_obj =
        {"code"    : code,
         "options" : compile_options(filename)};

    let response = await net.post_json_response(EPT_CompileContract, send_obj);
    return response;
}



async function
EncodeCalldata(code          : string,
               filename      : string,
               function_name : string,
               function_args : Array<string>)
    : Promise<Response>
{
    let send_obj =
        {"source"    : code,
         "options"   : compile_options(filename),
         "function"  : function_name,
         "arguments" : function_args};

    let response = await net.post_json_response(EPT_EncodeCalldata, send_obj);
    return response;
}



export {
    CompileContract,
    EncodeCalldata
}
