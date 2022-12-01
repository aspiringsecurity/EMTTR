# Evmos Smart Contract simple cli
This is a cli tool to deploy a smart contract to `evmos` chain plus a querying methods and sending transactions to a contract.

## Install
```bash
git clone https://github.com/mojtaba-esk/evmos-smart-contract.git
cd evmos-smart-contract
make install
```

to checkout if the tool is ready to use, run this command:
```bash
evmos-smart-contract --help
```
you should see the help info:
```bash
Deploy and manage a smart contract on evmos

Usage:
  evmos-smart-contract [command]

Available Commands:
  balance     Query the balance of a contract
  completion  Generate the autocompletion script for the specified shell
  deploy      Deploy a contract to the evmos chain
  help        Help about any command
  keys        Manage your applications keys
  query       Query a contract to the evmos chain
  transfer    Transfer a contract tokens to an address
  tx          Call a method of a contract that modifies the state

Flags:
      --algo string                      The algorithm used to generate the keys (default "eth_secp256k1")
      --chain-id string                  The evmos chain id (default "evmos_9000-1")
      --compiled-contracts-path string   The path to the compiled contracts in json format (default "/home/moji/mojimos/evmos-smart-contract/contracts/compiled_contracts")
  -h, --help                             help for evmos-smart-contract
      --keyring-backend string           Keyring backend to use, default value is: os (default "os")
      --keyring-dir string               Keyring backend directory (default "/home/moji/.evmosd")
      --node string                      The evmos node to connect to (default "http://localhost:8545")
      --output string                    Output format (text|json) (default "text")

Use "evmos-smart-contract [command] --help" for more information about a command.
```

## Running a local `evmos` node
First you need to get the evmos daemon binary.

```bash
git clone https://github.com/evmos/evmos.git
cd evmos
make install
```
If you face some errors make sure you have the prerequisites installed and configured properly. Checkout the [evmos repo](https://github.com/evmos/evmos/)

Check if `evmosd` is ready to use:
```bash
evmosd version
```

Then we go to the cli directory and initialize the local testnet
```bash
cd evmos-smart-contract
./init.sh
```
it will create an account and fills it up with funds and starts a local evmos testnet.

## Commands

### List the available accounts:
```bash
evmos-smart-contract keys list --keyring-backend test
```
since we are using a test environment, we use `--keyring-backend test` in all commands.

### Deploy a smart contract

The simplest way to deploy a smart contract is to put the solidity files inside `contracts` directory and run the following commands.

First we make sure we have all the required tools installed on our machine by running this command:

```sh
make contract-tools
```
After having the contract tools installed, let's compile all the contracts:

```bash
make contracts-compile
```

Now we can deploy the smart contract that we want. Here is the usage:
```sh
evmos-smart-contract deploy [contract_name] [init_params_json] [flags]
```

Let's deploy the `HelloWorld` contract:

```sh
evmos-smart-contract deploy HelloWorld '{"params":["Ciao Gholi"]}' --from mykey --keyring-backend test
```
where 
- `HelloWorld` is the name of the contract
- `'{"params":["Ciao Gholi"]}'` is the initial parameter(s) that the constructor of the contract receives in json format. 
**Note**: It only supports basic types including numbers and strings.
- `mykey` is the name of our key that the `init.sh` script has created for us and is funded

The output of this command is a contract address and a TX hash.
```yaml
Contract Address:  0x4c97c555477A96D4FA14d6F2B39ea1F6cDA5082a
TX Hash:  0x8cfb8907f6f715dc79ba7841567766b5a64505304d665628e873f1c94685f205
```

This command has a number of flags to make it more customized if we need:
```bash
--algo string : The algorithm used to generate the keys (default "eth_secp256k1")
--chain-id string : The evmos chain id (default "evmos_9000-1")
--compiled-contracts-path string : The path to the compiled contracts in json format (default "~/evmos-smart-contract/contracts/compiled_contracts")
--keyring-backend string : Keyring backend to use, default value is: os (default "os")
--keyring-dir string : Keyring backend directory (default "/home/moji/.evmosd")
--node string  : The evmos node to connect to (default "http://localhost:8545")
```

### Query a smart contract
The query command calls a method of a contract which only reads the state. i.e. does not change the state.

Here is the usage:
```sh
evmos-smart-contract query [contract_name] [contract_address] [method_to_call] [method_params_json] [flags]
```

Let's read the `message` value from the `HelloWorld` contract.

```bash
evmos-smart-contract query HelloWorld 0x4c97c555477A96D4FA14d6F2B39ea1F6cDA5082a message
```
where 
- `HelloWorld` is the name of the contract
- `0x4c97c555477A96D4FA14d6F2B39ea1F6cDA5082a` is the contract address
- `message` is the name of the method to call

Here is the example output:
```bash
output: [Ciao Gholi]
```

If we need to pass some parameters, in basic types, we can write it in json format after the name of the contract.

### Send a custom transaction to a contract
With `tx` command we can call a method of a contract that modifies the state. Here is the usage:

```sh
evmos-smart-contract tx [contract_name] [contract_address] [method_to_call] [method_params_json] [flags]
```

Let's update the `HelloWrold` message:
```sh
evmos-smart-contract tx HelloWorld 0x4c97c555477A96D4FA14d6F2B39ea1F6cDA5082a update '{"params":["Ciao bello"]}' --from mykey --keyring-backend test
```
The output is some information about the transaction:
```json
TX Hash:  0xa55aea8053c1eb3e33fa362a812d4949bc2da2ced7af47c8be506347cfb6b10b
{
  "type": "0x0",
  "nonce": "0x3",
  "gasPrice": "0xf4240",
  "maxPriorityFeePerGas": null,
  "maxFeePerGas": null,
  "gas": "0x2dc6c0",
  "value": "0x0",
  "input": "0x3d7403a30000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000a4369616f2062656c6c6f00000000000000000000000000000000000000000000",
  "v": "0x4674",
  "r": "0x7eb3bc59b355124bb7fc1db706537517110afaed29ed471c254b1406e0336019",
  "s": "0xbd8f130b58ed6654112d57d3fda5d7243c11ed93edaa95858b7771212dcfb59",
  "to": "0x4c97c555477a96d4fa14d6f2b39ea1f6cda5082a",
  "hash": "0xa55aea8053c1eb3e33fa362a812d4949bc2da2ced7af47c8be506347cfb6b10b"
}
```

Now we can query the message again to see if the update is reflected properly:
```bash
evmos-smart-contract query HelloWorld 0x4c97c555477A96D4FA14d6F2B39ea1F6cDA5082a message

output: [Ciao bello]
```

### Transfer transfer token balances on the deployed smart contract
To run this command we need a smart contract with ERC20 standard. Let's deploy one.

```bash
evmos-smart-contract deploy MyTestToken --from mykey --keyring-backend test
```
```yaml
Contract Address:  0xe97faB40A966BF149B0926008AbA2aB854b16A1d
TX Hash:  0x654d2b28e3bfe9a84bb781e3d86099247c49976eeb3e402d36f52e1a493db03c
```

Here is the usage of the transfer command:
```bash
evmos-smart-contract transfer [contract_name] [contract_address] [to_address] [amount] [flags]
```

Let's transfer some tokens:
```sh
evmos-smart-contract transfer MyTestToken 0xe97faB40A966BF149B0926008AbA2aB854b16A1d e0x25f59737df93dc4e1a6e55c86e87d898e8f760c1 10 --from mykey --keyring-backend test
```
where
- `MyTestToken` is the name of the contract
- `0xe97faB40A966BF149B0926008AbA2aB854b16A1d` is the contract address
- `e0x25f59737df93dc4e1a6e55c86e87d898e8f760c1` is the receiver account
- `10` is the amount to transfer
- `mykey` is the sender account

Here is the output:
```json
TX Hash:  0x79b17dfc8465bc092f7f4132b84e58eaed62ecab94026077e3f76f63ddb57370
{
  "type": "0x0",
  "nonce": "0x5",
  "gasPrice": "0xf4240",
  "maxPriorityFeePerGas": null,
  "maxFeePerGas": null,
  "gas": "0x2dc6c0",
  "value": "0x0",
  "input": "0xa9059cbb000000000000000000000000000000000000000000000000000000000000000e000000000000000000000000000000000000000000000000000000000000000a",
  "v": "0x4673",
  "r": "0x364037b33b24b70dbec42b3291f96395a2797e70629cca8a88cb05a0d3a14534",
  "s": "0x75415693024db69df881beef0968458c2806fa84842599147e76fa4c2e1fcf50",
  "to": "0xe97fab40a966bf149b0926008aba2ab854b16a1d",
  "hash": "0x79b17dfc8465bc092f7f4132b84e58eaed62ecab94026077e3f76f63ddb57370"
}
```

### Query the token balances on the deployed smart contract
Here is the command usage:
```sh
evmos-smart-contract balance [contract_name] [contract_address] [account_address] [flags]
```
Let's check the balance of the account that we just did the transfer to:
```bash
evmos-smart-contract  balance MyTestToken 0xe97faB40A966BF149B0926008AbA2aB854b16A1d e0x25f59737df93dc4e1a6e55c86e87d898e8f760c1
```
```yaml
Amount: 10.000000000000000000
Denom: MTT
```

## Test
To run integration test use the following command:
```sh
make integration-test
```
It runs a local evmos node itself and performs the tests.

## Code docs
https://pkg.go.dev/github.com/mojtaba-esk/evmos-smart-contract