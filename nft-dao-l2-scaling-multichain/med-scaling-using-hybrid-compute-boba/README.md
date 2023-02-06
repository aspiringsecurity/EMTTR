# Hybrid-Compute for Medical Research University on the Boba Network. 

## Quick start: Testnet setup
1. Install dependencies with `yarn` or `npm i`
2. Copy `.env.example`, rename to `.env` and add one of your private keys
3. Get Boba testnet tokens from [Gateway Rinkeby](https://gateway.rinkeby.boba.network/)
4. Fill in the TODO's in the `Calculator.sol:calcTimeDilation` function.
5. Deploy your contract incl. the tests, e.g. `yarn run test:rinkeby`

NOTE: The backend has already been deployed for this workshop. 

### The flow
When creating a new Hybrid-Compute project you should be aware of the following required steps to use it: 
1. Deploy your own `TuringHelper.sol` - This is needed to introduce AccessControl and to keep track of your calls (every call costs 0.01 BOBA token).
2. Deploy your own custom contract (in our case `Calculator.sol`) and provide the address of your deployed TuringHelper.
3. Call `addPermittedCaller(address)` on your TuringHelper contract to add your custom contract (previous step). This is needed to whitelist your contract to use your prepaid BOBA tokens when calling HybridCompute. 
4. Now we need to fund our TuringHelper with BOBA tokens since every call costs **0.01 BOBA** tokens. You can do this by calling `turingCredit.addBalanceTo(uint256,address)`. 

**Notes to 4):**
* You can find the contract address for the TuringCredit contract [here](https://docs.boba.network/for-developers/network-parameters) (this is deployed by the BOBA team once).
* `turingCredit.addBalanceTo(uint256,address)`: The first param describes how many BOBA tokens you want to send to the contract (pre-pay) and the second is the address of your `TuringHelper` contract.

### How to call a Hybrid-Compute function?
When calling a function on your smart-contract that utilizes Hybrid-Compute then you need to `estimateGas` before. 
Otherwise Hybrid-Compute won't be able to intercept the call. 

```js
await calcContract.estimateGas.calcTimeDilation(properTime, velocity, gasOverride)
const tx = calcContract.calcTimeDilation(properTime, velocity, gasOverride)
```


## Hybrid Compute
Turing is a system for interacting with the outside world from within solidity smart contracts. Ethereum is a computer with multiple strong constraints on its internal architecture and operations, all required for decentralization. As such, things that most developers take for granted - low cost data storage, audio and image processing, advanced math, millisecond response times, random number generation, and the ability to talk to any other computer - can be difficult or even impossible to run on the Ethereum "CPU". Of course, the benefits of decentralization far outweigh those limitations, and therefore, tools are desirable to add missing functionality to the Ethereum ecosystem. Turing is one such tool.

Turing is a **pipe** between (**1**) Boba's Geth (aka sequencer), which takes transactions, advances the state, and forms blocks, and (**2**) your server. To use this pipe, all you need is a smart contract on Boba that makes Turing calls and an external server that accepts these calls and returns data in a format that can be understood by the EVM. This is not hard to do and we provide many examples which will allow you to quickly build a working Turing system.

* Official examples - [[here]](https://github.com/bobanetwork/boba/tree/develop/boba_examples)
* Official documentation - [[here]](https://docs.boba.network/turing)

## Core components
All Hybrid Compute projects (synonymous for Turing) usually consist of 3 core components: 
* The main smart contract where you put your on-chain logic (this is the `Calculator.sol` in our case),
* The backend / API where you put your off-chain logic (in this case `calculator.py`),
* And the `TuringHelper.sol which you basically just can copy, paste and deploy. This is needed to add some accessControl logic and to keep track of your calls since you are being charged by call. 

`\contracts\Calculator.sol` [[here]](contracts/Calculator.sol): Our main smart-contract that contains a function that expects the `properTime` and `velocity` parameter and triggers our API endpoint with those parameters.  
```solidity 
calcTimeDilation(uint256 properTime, uint256 velocity)
```

`\aws\calculator.py` [[here]](aws/calculator.py): Backend API - this endpoint will be called by our smart-contract.

`\contracts\TuringHelper.sol` [[here]](contracts/TuringHelper.sol): Official BOBA standard (no changes needed). This contract needs to be deployed to add some kind of access control (which contract is allowed to make requests) and to allow the TuringCredit contract to keep track of API requests that have been sent since every call costs 0.01 BOBA.  

### Other interesting files

`\aws\template.yaml` [[here]](aws/template.yaml): We are usually using [AWS](https://aws.amazon.com/) for our HybridCompute services. This is a so-called [SAM-template](https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/sam-specification.html) that you can use to deploy the backend on your own backend automatically.

`\test\Calculator.ts` [[here]](test/Calculator.ts): This file just contains some basic test-cases to verify that your stack is working. 

`\scripts\deploy.ts` [[here]](scripts/deploy.ts): Deploy your smart-contract(s). 

`\workshop-examples\**\*` [[here]](workshop-examples): This folder contains some further examples that you could dig into. 
Speaking of examples you could also have a look at [these examples](https://github.com/bobanetwork/boba/tree/develop/boba_examples).

## Background
This project calculates the time dilation according to Albert Einstein's special relativity theory. Please find the formula below.

$$ observerTime = { properTime \over \sqrt{1 - ({velocity \over speedOfLight})^2}} $$

Basically this states that time can pass at different rates in different reference frames. The time depends on the velocity of one reference frame relative to another. 

For us this basically just means we can calculate how the time is diluted from our perspective (the observer) when an object moves at a specific speed.

You can find the implementation of this formula in `\aws\calculator.py:calc_result()`.





