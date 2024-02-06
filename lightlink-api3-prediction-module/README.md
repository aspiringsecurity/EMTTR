## Blockscout Rust Services

Blockscout interactions with Lightlink modules and EMTTR Modules: Blockscout provides a comprehensive, easy-to-use interface for users to view, confirm, inspect and interact on EVM (Ethereum Virtual Machine) blockchains.  We are developing 2 transparent tools for OP Medicine, which are needed to analyze and validate OP Medicine transactions. Tracing Optimism transactions to improve user retention and detect, analyze issues in real-time: trace optimism transactions in DICOM NFT marketplace for enabling data authenticity and transparency. Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/lightlink-api3-prediction-module/blockscout-rs

OP MED: Low code analytics framework tool for Medication Log. Secure data storage, transparent data movement and data authenticity using OP low code tooling and Blockscout analytics.

Optimism TestXLS: Patient’s visit and Diagnosis Tooling with Blockscout based analytics tool for validation, ionic framework within OP NFT marketplace.


## Predictions Module using Lightlink and dAPIs

A Predictions Module that lets users bet on the outcome of future events. Powered by API3 dAPIs.

Users can come in and vote on the price of an asset at a future date. The market will then use the API3 dAPIs to get the price of the asset at the future date and distribute the rewards to the users who voted correctly.

Users can also come in and make their own markets. They can set the asset, the price of the token and the dAPI they're referencing to get the price of the asset at the future date.

## Structure

- `/backend` contains all the protocol contracts and scripts to run/deploy and interact with the market.
- `/frontend` contains the UI for the market. Built using NextJS.
- `/graph` contains the subgraph for the market. This is used to track market events. Built using The Graph.
