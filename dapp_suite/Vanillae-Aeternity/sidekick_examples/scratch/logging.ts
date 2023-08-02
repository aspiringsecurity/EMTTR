import * as sk from './jx_include/ppr-sidekick-0.1.0/dist/sidekick.js'

let logger = new sk.ConsoleLogger();
logger.listen(window);

window.postMessage("I have questions regarding my vehicle's extended warranty.");
