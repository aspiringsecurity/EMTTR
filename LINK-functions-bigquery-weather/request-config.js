const {
  Location,
  ReturnType,
  CodeLanguage,
} = require("@chainlink/functions-toolkit");

const fs = require("fs");
require("@chainlink/env-enc").config()
// require('dotenv').config()

// Configure the request by setting the fields below
const QUERY = `SELECT * FROM bigquery-public-data.noaa_gsod.gsod2023 where stn = '081810' order by date desc limit 1`;

const requestConfig = {
  // String containing the source code to be executed
  source: fs.readFileSync("./BigQuery-weather.js").toString(),
  // Location of source code (only Inline is currently supported)
  codeLocation: Location.Inline,
  // Optional. Secrets can be accessed within the source code with `secrets.varName` (ie: secrets.apiKey). The secrets object can only contain string values.
  secrets: {
    key: process.env.GOOGLE_KEY,
    iss: process.env.GOOGLE_ISS,
    projectId: process.env.GOOGLE_PROJECT_ID,
    authToken: ""  // set in the encrypt-secrets taskArgs.
  },
  // Optional if secrets are expected in the sourceLocation of secrets (only Remote or DONHosted is supported)
  secretsLocation: Location.DONHosted,
  // Args (string only array) can be accessed within the source code with `args[index]` (ie: args[0]).
  args: [QUERY],
  // Code language (only JavaScript is currently supported)
  codeLanguage: CodeLanguage.JavaScript,
  // Expected type of the returned value
  expectedReturnType: ReturnType.uint256,
};



module.exports = requestConfig;
