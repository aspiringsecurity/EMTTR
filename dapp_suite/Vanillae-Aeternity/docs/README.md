# The Vanillae Files

My name is Peter Harpending.  I'm one of the two developers of the Vanillae
Project (other: Craig Everett).

Craig and I started by creating [Aegora.jp: a simple e-commerce website that
uses Aeternity as its payment system.](https://aegora.jp)  We are also
currently developing a wallet.  We have run into many weird stupid technical
pitfalls or weird things you have to learn about that take like 8 hours but
really are 1 hour of complexity.  Solving these pitfalls was the genesis of the
Vanillae project.  This file tree documents the pitfalls we cared to write about.

Most of these have code examples in Erlang and/or TypeScript.

This is roughly the order in which you are likely to encounter these pitfalls,
but each document is self-contained (ish) \[non-links are planned and have not
been written yet\]:

1. [How to install NPM without getting AIDS](./npm-misc/)
2. [Base64 versus Base58 with code in Erlang and TypeScript](./baseN/)
3. [Ethereum Recursive Length Prefix (RLP) Encoding, with code in Erlang and TypeScript](./rlp/)
4. [Keccak, SHA-3, and SHAKE-N, with code in Erlang](./kek/)
5. Seed Phrase Recovery, with code in Erlang and TypeScript
6. Fast Aeternity Recovery Text (FÆRT): a better seed phrase standard
7. Sidekick and the Aepp-Waellet Communication Protocol (AWCP): how to talk to browser wallet extensions from a page script (and vice versa)
8. How browser extensions work
9. Basics of Elliptic Curve Cryptography
10. How to build a wallet
11. Aeternity Interoperable Device Signatures (AIDS): a standard for decentralized point-of-sale services (think Square, Stripe, etc)
12. vanillae-erl: how to talk to the blockchain from your service backend
13. How to run an Aeternity node
14. The Jex Packaging System: NPM is cancer
15. Things we learned making Aegora
16. Using user crypto wallets for authentication: it's time to put passwords in the past
