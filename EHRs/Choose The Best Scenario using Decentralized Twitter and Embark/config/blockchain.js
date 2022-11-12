module.exports = {
  development: {
    enabled: true,
    networkType: "custom",
    networkId: "1337",
    isDev: true,
    genesisBlock: "config/development/genesis.json",
    datadir: ".embark/development/datadir",
    mineWhenNeeded: true,
    nodiscover: true,
    maxpeers: 0,
    rpcHost: "localhost",
    rpcPort: 8545,
    rpcCorsDomain: "auto",
    rpcApi: ['eth', 'web3', 'net', 'debug', 'personal'],
    proxy: true,
    account: {
      password: "config/development/password",
      numAccounts: 3,
      balance: "5 ether"
    },
    targetGasLimit: 10000000000,
    wsOrigins: "auto",
    wsRPC: true,
    wsHost: "localhost",
    wsPort: 8546,
    wsApi: ['eth', 'web3', 'net', 'shh', 'debug', 'personal'],
    simulatorMnemonic: "example exile argue silk regular smile grass bomb merge arm assist farm",
    simulatorBlocktime: 0
  },
  testnet: {
    enabled: true,
    networkType: "testnet",
    light: true,
    rpcHost: "localhost",
    rpcPort: 8545,
    rpcCorsDomain: "http://localhost:8000",
    account: {
      password: "config/testnet/password"
    }
  },
  livenet: {
    enabled: true,
    networkType: "livenet",
    light: true,
    rpcHost: "localhost",
    rpcPort: 8545,
    rpcCorsDomain: "http://localhost:8000",
    account: {
      password: "config/livenet/password"
    }
  },
  privatenet: {
    enabled: true,
    networkType: "custom",
    rpcHost: "localhost",
    rpcPort: 8545,
    rpcCorsDomain: "http://localhost:8000",
    datadir: "yourdatadir",
    networkId: "123",
    bootnodes: ""
  }
}
