
// Needed to create Oauth token for BigQuery call
const QUERY = args[0];

const JWT_HEADER = '{"alg":"RS256","typ":"JWT"}';
const TOKEN_URL = "https://oauth2.googleapis.com/token";
const AUDIENCE = "https://oauth2.googleapis.com/token";
const SCOPE = "https://www.googleapis.com/auth/cloud-platform.read-only";
const BIGQUERY_BASE_URL =
  "https://bigquery.googleapis.com/bigquery/v2/projects/";

const OAUTH_TOKEN = secrets.authToken

const executeQuery = async query => {
  const requestConfig = {
    method: "post",
    url: `${BIGQUERY_BASE_URL}${secrets.projectId}/queries`,
    headers: {
      Authorization: `Bearer ${OAUTH_TOKEN}`,
      Accept: "application/json",
      "Content-Type": "application/json",
    },
    data: {
      query,
      useLegacySql: false,
    },
  };

  const response = await Functions.makeHttpRequest(requestConfig);

  if (!response.data) {
    throw new Error("Invalid response from BigQuery");
  }

  const rows = response.data.rows;

  let answer;
  try {
    answer = parseFloat(rows[0].f[6].v); // grab the temp from the response
  } catch (error) {
    throw new Error(`Error processing query result: ${error.message}`);
  }

  return Functions.encodeUint256(BigInt(answer * 10 ** 18));
};

const result = await executeQuery(QUERY);
return result;
