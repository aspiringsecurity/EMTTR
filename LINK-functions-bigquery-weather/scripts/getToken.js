const crypto = require("crypto");
const querystring = require("querystring");
const axios = require("axios");
const fs = require("fs");

require("@chainlink/env-enc").config();
// require('dotenv').config()

const G_KEY = process.env.GOOGLE_KEY;
const G_ISS = process.env.GOOGLE_ISS;
const JWT_HEADER = '{"alg":"RS256","typ":"JWT"}';
const TOKEN_URL = "https://oauth2.googleapis.com/token";
const AUDIENCE = "https://oauth2.googleapis.com/token";
const SCOPE = "https://www.googleapis.com/auth/cloud-platform.read-only";

const getOauthToken = async (iss, key) => {
  const jwt = createJWT(iss, key);

  const jwtRequest = {
    grant_type: "urn:ietf:params:oauth:grant-type:jwt-bearer",
    assertion: jwt,
  };

  const jwtRequestString = querystring.stringify(jwtRequest);

  try {
    const tokenResponse = await axios.post(TOKEN_URL, jwtRequestString);
    const requestStatus = tokenResponse.status;
    if (requestStatus !== 200) {
        throw Error (`Error fetching OAuth token: HTTP Status is ${requestStatus}`)
    }
    return tokenResponse.data.access_token;
  } catch (error) {
    console.error("Error fetching OAuth token:", error);
    throw new Error("Error fetching OAuth token");
  }
};

const createJWT = (
  iss = G_ISS,
  key = G_KEY,
  scope = SCOPE,
  audience = AUDIENCE
) => {
  const privateKey = key.replace(/\\n/g, "\n");
  const currentTimeInSeconds = Math.round(Date.now() / 1000);

  const jwtClaimSetObj = {
    iss,
    scope,
    aud: audience,
    exp: currentTimeInSeconds + 3500,
    iat: currentTimeInSeconds,
  };

  const jwtBase64Headers = Buffer.from(JWT_HEADER).toString("base64");
  const jwtBase64ClaimSet = Buffer.from(
    JSON.stringify(jwtClaimSetObj)
  ).toString("base64");
  const stringToSign = `${jwtBase64Headers}.${jwtBase64ClaimSet}`;
  const jwtBase64Signature = crypto
    .sign("RSA-SHA256", stringToSign, privateKey)
    .toString("base64");

  const base64Jwt = `${jwtBase64Headers}.${jwtBase64ClaimSet}.${jwtBase64Signature}`;
  return base64Jwt;
};

getOauthToken(G_ISS, G_KEY).then(token => console.log("Token fetched OK:  ", token)).catch(err => {
  console.log("Error fetching token    ", err);
});

