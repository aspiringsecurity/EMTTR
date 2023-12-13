const { decodeResult } = require("@chainlink/functions-toolkit");
const path = require("path");
const process = require("process");
// const  {ethers} =  require("ethers");

task(
  "read-weather",
  "Reads the latest response (or error) returned to a FunctionsConsumer  consumer contract"
)
  .addParam("contract", "Address of the consumer contract to read")
  .addOptionalParam(
    "configpath",
    "Path to Functions request config file",
    `${__dirname}/../../Functions-request-config.js`,
    types.string
  )
  .setAction(async taskArgs => {
    console.log(
      `Reading data from FunctionsBigQueryConsumer contract ${taskArgs.contract} on network ${network.name}`
    );
    const consumerContractFactory = await ethers.getContractFactory(
      "FunctionsBigQueryConsumer"
    );
    const consumerContract = await consumerContractFactory.attach(
      taskArgs.contract
    );

    let latestError = await consumerContract.s_lastError();
    if (latestError.length > 0 && latestError !== "0x") {
      const errorString = Buffer.from(latestError.slice(2), "hex").toString();
      console.log(`\nOn-chain error message: ${errorString}`);
    }

    let latestResponse = await consumerContract.s_lastWeather();
    if (latestResponse.length > 0 && latestResponse !== "0x") {
      const requestConfig = require(path.join(
        process.cwd(),
        "/request-config.js"
      ));

      const decodedResultWei = decodeResult(
        latestResponse,
        requestConfig.expectedReturnType
      )
      const formatted = ethers.utils.formatEther(decodedResultWei)
      console.table({
        "Response hex": latestResponse,
        "Response hex decoded":formatted ,
      });

console.log( `\nWelcome to SmartCon 2023.  The weather in Barcelona today is: ${formatted} degrees Fahrenheit. Enjoy the conference!\n\n`)
    } else if (latestResponse == "0x") {
      console.log("Empty response: ", latestResponse);
    }
  });
