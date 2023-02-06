require("@matterlabs/hardhat-zksync-deploy");
require("@matterlabs/hardhat-zksync-solc");

import sanitizedConfig from './config';

module.exports = {
    zksolc: {
        version: "1.2.0",
        compilerSource: "binary",
        settings: {
            experimental: {
                dockerImage: "matterlabs/zksolc",
                tag: "v1.2.0",
            },
        },
    },
    zkSyncDeploy: {
        zkSyncNetwork: "https://zksync2-testnet.zksync.dev",
        ethNetwork: sanitizedConfig.API_ENDPOINT,
    },
    networks: {
        hardhat: {
            zksync: true,
        },
    },
    solidity: {
        version: "0.8.16",
    },
};
