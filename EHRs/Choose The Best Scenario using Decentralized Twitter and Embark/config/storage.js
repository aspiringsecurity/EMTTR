module.exports = {
  default: {
    enabled: true,
    ipfs_bin: "ipfs",
    provider: "ipfs",
    available_providers: ["ipfs"],
    upload: {
      provider: "ipfs",
      host: "localhost",
      port: 5001,
      getUrl: "http://localhost:8080/ipfs/"
    },
    dappConnection: [
      {provider: "ipfs", host: "localhost", port: 5001, getUrl: "http://localhost:8080/ipfs/"},
      {provider: "swarm", host: "localhost", port: 5001, getUrl: "http://localhost:8080/ipfs/"}
    ]
  },
  development: {
    enabled: true,
    provider: "ipfs",
    upload: {
      host: "localhost",
      port: 5001,
      getUrl: "http://localhost:8080/ipfs/"
    }
  }
}
