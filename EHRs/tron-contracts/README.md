# tron-contracts
Solidity smart contracts for the [TRON](https://tron.network) blockchain configured with [TronBox](https://github.com/tronprotocol/tron-box).


## Usage

### Install tronbox:

```npm install -g tronbox```

### Clone repo:

```git clone https://github.com/tronprotocol/tron-contracts.git```

### Import Address and Private Key:

Import an address and private key of an account into the "from" and "privateKey" fields inside the tronbox.js file. Make sure it has test TRX to deploy the contracts. You can request some [here](https://www.trongrid.io/shasta/#request).

### Compile:

```tronbox compile```

### Migrate:

```tronbox migrate --reset```


## License

Released under the [MIT License](LICENSE).
