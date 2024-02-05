const CRON_KEY = process.env.EASY_CRON_API_KEY;
const axios = require("axios");

// The current scheme of things :
// We get the predictionId and when its supposed to end. Call the cron-job.org api and create
// a cron job that is called at the end time. At the day of conclude : calls the conclude_prediction_1
// at the Settlement.sol with the originally set prediction_id.
/// NEED TO ADD THE PREDICTION ID IN THE URL ITSELF. EXTRACT THE PREDICTION ID FROM THE URL.

async function createCRONURL(predictionId) {
  return `https://d-api-prediction-market.vercel.app/api/conclude_prediction?predictionId=${predictionId}`;
}

async function getCRONExpression(timestamp) {
  const date = new Date(timestamp * 1000); // Convert seconds to milliseconds

  // Extract values from the date
  const seconds = date.getSeconds();
  const minutes = date.getMinutes();
  const hours = date.getHours();
  const dayOfMonth = date.getDate();
  const month = date.getMonth() + 1; // Months in JavaScript are 0-indexed
  const year = date.getFullYear();

  // Create the cron expression
  return `${seconds} ${minutes} ${hours} ${dayOfMonth} ${month} * ${year}`;
}

async function createCRONJob(id, timestamp) {
  const url = await createCRONURL(id);
  const expression = await getCRONExpression(timestamp);

  const headers = {
    "Content-Type": "application/json",
    Authoraization: `Bearer ${CRON_KEY}`,
  };

  const jobData = {
    url: url,
    time: expression, // Cron schedule
  };

  axios
    .put("https://api.cron-job.org/jobs", jobData, { headers })
    .then((response) => {
      console.log("Cron job created:", response.data);
    })
    .catch((error) => {
      console.error("Error creating cron job:", error);
    });

  return [expression, url];
}

export default async function handler(req, res) {
  if (req.method == "POST") {
    const data = req.body;

    const { predictionId, endingTimestamp } = data;

    const success = await createCRONJob(predictionId, endingTimestamp);

    if (success) res.status(200).json({ url: success[0], cron: success[1] });
    else res.status(400).json({ error: "Can't create CRON job." });
  }
}
