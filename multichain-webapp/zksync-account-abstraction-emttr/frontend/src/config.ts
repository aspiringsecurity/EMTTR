import AA_FACTORY_ARTIFACT from "./chain/artifacts-zk/contracts/AAFactory.sol/AAFactory.json";
import AA_ARTIFACT from "./chain/artifacts-zk/contracts/PluginableAccount.sol/PluginableAccount.json";
import LIMITER_PLUGIN_ARTIFACT from "./chain/artifacts-zk/contracts/LimiterPlugin.sol/Plugin.json";
import GREETER_ARTIFACT from "./chain/artifacts-zk/contracts/Greeter.sol/Greeter.json";

const AA_FACTORY_ADDRESS = "0x726777c680D56b6Cb67DFedc7F0Cf51795e56499";
const LIMITER_PLUGIN_ADDRESS = "0x5C460B5c76275C61d142b238AF59B6e20c155BCC";
const GREETER_ADDRESS = "0xA47B01Cfb50e668dA4a1b0Ae2fFE56dca34Cae65";

let ZKSYNC_URL_PROVIDER = "https://zksync2-testnet.zksync.dev";

export {
    AA_FACTORY_ARTIFACT,
    AA_ARTIFACT,
    LIMITER_PLUGIN_ADDRESS,
    LIMITER_PLUGIN_ARTIFACT,
    AA_FACTORY_ADDRESS,
    GREETER_ADDRESS,
    GREETER_ARTIFACT,
    ZKSYNC_URL_PROVIDER,
};
