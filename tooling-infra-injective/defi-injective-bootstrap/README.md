### Pre-Requisites

* Unix
* Terminal
* Git 2.37.1 or above

## Tutorial

### Section 0: Getting Started

First clone the repo:

```bash
$ git clone https://github.com/InjectiveLabs/cosmwasm101.git
$ cd cosmwasm101
$ git checkout section-0
```

Here you find an empty repo, with nothing but this `README.md` to see. If you got this far you probably have got the relevant pre-requisites installed.

Navigate to sections 1 in order to find the instructions to get the environment setup:

```bash
$ git checkout section-1
```

### Section 1: Environment Setup

Now setup the development environment. You will need to install:

* [Rust](https://www.rust-lang.org/tools/install)
* CosmWasm
* [Cargo Generate](https://github.com/cargo-generate/cargo-generate)
* [PlantUML](https://plantuml.com/)

First download rust:

```bash 
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
$ rustup default stable
```

Check rust has successfully been installed:

```bash
$ cargo version
$ cargo 1.67.1 (8ecd4f20a 2023-01-10)
```

**Note:** If version is lower than 1.55 we need to update run command `$ rustup update stable`

Next add the CosmWasm targets:

```bash
$ rustup target add wasm32-unknown-unknown
```

Finally install `cargo generate` this will help us quickly launch a new CosmWasm project.

```bash
$ cargo install cargo-generate --features vendored-openssl
$ cargo install cargo-run-script
```

#### Recommendec IDE Plugins

You may wish you install helpful plugins for your IDE:

* [VS Code: Rust Analyzer](https://marketplace.visualstudio.com/items?itemName=rust-lang.rust-analyzer)
* [Jet Brains](https://www.jetbrains.com/rust/)

### Section 2: Generate CosmWasm Contract

Once rust, CosmWasm and cargo generate have successfully been installed it is possible to create a contract skeleton.

In the terminal 

```bash
$ cargo generate --git https://github.com/CosmWasm/cw-template.git --name CosmWasm -d minimal=true
```

Now you should have a directory similar to the following:

```bash
.
├── README.md
└── cosmwasm
    ├── Cargo.toml
    ├── LICENSE
    ├── NOTICE
    └── src
```

### Section 3: Design

Now let's use [PlantUML](https://plantuml.com/) to make some component and sequence diagrams. After following installation of PlantUML create a new directory and files as below:

```bash
$ mkdir diagrams
$ touch diagrams/component-diagram.puml && touch diagrams/instantiate-sequence.puml && touch diagrams/escrow-sequence.puml && touch diagrams/query-sequence.puml
```

Following the template shown in Plantuml documentation create a component diagram and sequence diagrams for the following scenario:

* instantiation
* escrow and redemption
* queries

### Section 4: Escrow Contract Framework

Having created diagrams describing the basic functionality of the Escrow contract implement the following:

* Instantiate
* Execute
    * Escrow
    * Redemption
* Query
    * Config
    * Escrow State

First define the storage of the contract which should be config and then a storage containing information about a user's specific storage. To do this use Rust crate [CosmWasm Storage Plus][1].

The config should store:

* CW20 token address
* Contract owner address

The escrow state should store:

* User address
* Amount escrowed
* Time of escrow

### Section 5: Unit Testing

Once the storage has been correctly implemented and additional query functions we are able to start to develop unit tests. Unit tests are of course the first step towards building good software.

Create a file `unit_tests.rs` and using the [cosmwasm-std testing module](https://docs.rs/cosmwasm-std/1.2.2/cosmwasm_std/testing/index.html) we can build a unit test for the contract instantiation and query functions created in the previous section. In addition we will import the `instantiate` and `query` from the escrow contract itself.

Place this skeleton in `unit_tests.rs`:

```
use crate::contract::{execute, instantiate, query};
use cosmwasm_std::testing::{mock_dependencies, mock_env, mock_info};

#[test]
fn test_instantiation() {
    // FILL THIS IN 
}
```

### Section 6: Integration Testing

Having successfully created some unit tests we wish to start developing the execute functions for escrow and redemption. However, as they involve a CW20 token it will not be possible to create these tests as unit tests. Rather we will need to create a mock blockchain environment and create integration tests.

To do this we will leverage the [cw-multi-test package](https://docs.rs/cw-multi-test/0.16.2/cw_multi_test/).

In order to complete this section we will:

* Add `cw-multi-test` as a dev dependency in our `Cargo.toml`
* Create the functions:
    * Escrow: should allow a user to send CW20 token to the escrow contract which receives and locks the tokens a period of time
    * Redeem: should allow a user to redeem their locked tokens once the period has expired
* Use `cw-multi-test` to create integration tests

## Resources

[1]: https://github.com/CosmWasm/cw-storage-plus