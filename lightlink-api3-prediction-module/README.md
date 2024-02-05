## Predictions Module using Lightlink and dAPIs

A Predictions Module that lets users bet on the outcome of future events. Powered by API3 dAPIs.

Users can come in and vote on the price of an asset at a future date. The market will then use the API3 dAPIs to get the price of the asset at the future date and distribute the rewards to the users who voted correctly.

Users can also come in and make their own markets. They can set the asset, the price of the token and the dAPI they're referencing to get the price of the asset at the future date.

## Structure

- `/backend` contains all the protocol contracts and scripts to run/deploy and interact with the market.
- `/frontend` contains the UI for the market. Built using NextJS.
- `/graph` contains the subgraph for the market. This is used to track market events. Built using The Graph.