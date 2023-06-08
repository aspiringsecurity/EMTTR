const fs = require("fs")

// Loads environment variables from .env.enc file (if it exists)
require("@chainlink/env-enc").config()

const Location = {
  Inline: 0,
  Remote: 1,
}

const CodeLanguage = {
  JavaScript: 0,
}

const ReturnType = {
  uint: "uint256",
  uint256: "uint256",
  int: "int256",
  int256: "int256",
  string: "string",
  bytes: "Buffer",
  Buffer: "Buffer",
}

const isoStartDate = "2023-02-12T12:00:00+0000";
const numberOfWinners = '3';

// Configure the request by setting the fields below
const requestConfig = {
  // location of source code (only Inline is currently supported)
  codeLocation: Location.Inline,
  // code language (only JavaScript is currently supported)
  codeLanguage: CodeLanguage.JavaScript,
  // string containing the source code to be executed
  source: fs.readFileSync("./Functions-request-source.js").toString(),
  // Secrets can be accessed within the source code with `secrets.varName` (ie: secrets.apiKey). The secrets object can only contain string values.
  secrets: { FACEBOOK_PAGE_ID: process.env["FACEBOOK_PAGE_ID"], FACEBOOK_GRAPH_API_KEY: process.env["FACEBOOK_GRAPH_API_KEY"], TESTER_ACCOUNTS_IDS: process.env["TESTER_ACCOUNTS_IDS"] },
  // Per-node secrets objects assigned to each DON member. When using per-node secrets, nodes can only use secrets which they have been assigned.
  perNodeSecrets: [],
  // ETH wallet key used to sign secrets so they cannot be accessed by a 3rd party
  walletPrivateKey: process.env["PRIVATE_KEY"],
  // args (string only array) can be accessed within the source code with `args[index]` (ie: args[0]).
  args: [isoStartDate, numberOfWinners],
  // expected type of the returned value
  expectedReturnType: ReturnType.bytes,
  // Redundant URLs which point to encrypted off-chain secrets
  secretsURLs: [],
}

module.exports = requestConfig