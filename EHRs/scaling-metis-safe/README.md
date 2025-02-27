
## Getting Started

Install dependencies and start a local dev server.

```
npm install
npm start
```

Then:

- Go to the [Safe web interface](https://dev.gnosis-safe.io)
- Create your test safe
- Paste your localhost URL, default is http://localhost:3000/
- You should see Safe Starter as a new app
- Develop your app from there

## Features

Gnosis Safe App Starter combines recommendations described in the following repositories:

- [Safe Apps SDK](https://github.com/gnosis/safe-apps-sdk)
- [safe-react-components](https://github.com/gnosis/safe-react-components)

You can use the `useSafe` React hook to interact with the Safe Apps SDK

```
const safe = useSafe();
console.log(safe.info);
```

Safe React Components are also integrated and ready to use. [See all components](https://components.gnosis-safe.io/).

## Dependencies

### Included
- [`@gnosis.pm/safe-react-components`](https://github.com/gnosis/safe-react-components) (UI components themed for the Safe interface)
- [`@rmeissner/safe-apps-react-sdk`](https://github.com/rmeissner/safe-sdks-js/tree/master/safe-apps-react-sdk) (React hook for the Safe Apps SDK)

### Recommended
- [`ethers`](https://github.com/ethers-io/ethers.js) (Library for interacting with Ethereum)
- [`web3`](https://github.com/ethereum/web3.js/) (Library for interacting with Ethereum)
- [`@studydefi/money-legos`](https://github.com/studydefi/money-legos) (Library for DeFi interactions)
