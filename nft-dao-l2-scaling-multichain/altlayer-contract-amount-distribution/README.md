# CRO contract payment Management and Amount Distribution

---


EMTTR's CRO contract payment management and amount distribution simplifies managing payments through an automated amount prize distribution system. We are extending and adapting WenBounty project on AltLayer.

**Pharma companies can:**

- add money to a smart contract pool (escrow),
- create multiple channels,
- Increase track/overall bonus during the contract work
- Details about contract work and their offerings


--

## Repository Information

This monorepo contains the Front-end and the smart contracts for CRO contract payment management Project. The monorepo contains 2 directories:

### dapp

TS, React, Next.js, ethers.js, tailwind css

### contracts

Solidity, Hardhat, ethers.js, Mocha + TS for testing

# Getting started

To run the dapp:

_Make sure you're connected to AltLayer Dev Network_

```
cd dapp && yarn dev
```

To compile the smart contracts:

```
cd contracts
yarn hardhat compile
```

To run the tests:

```
cd contracts
yarn hardhat test
```


Our software stack:

- Typescript
- Solidity
- Mocha for testing
- Next
- React

---
