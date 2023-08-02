import * as sk from './jx_include/ppr-sidekick-0.1.0/dist/sidekick.js'

//var body_count = 1;
//
//function
//message_event_to_request
//    (evt : {data: object})
//{
//    let url    = 'http://localhost:8841/';
//    let body   = JSON.stringify(evt.data, undefined, 4);
//    let result = new Request(url, {method: 'POST', body: body});
//    return result;
//}

let cl = new sk.ConsoleLogger();
cl.listen(window);

let hl = new sk.HttpLogger('http://localhost:8841/');
hl.listen(window);

let logger = new sk.SeqLogger([cl, hl]);

window.postMessage("I have questions regarding my vehicle's extended warranty.");
