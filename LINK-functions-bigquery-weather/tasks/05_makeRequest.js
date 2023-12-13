const {
  CodeLanguage,
  SubscriptionManager,
  SecretsManager,
  simulateScript,
  decodeResult,
  ResponseListener,
  ReturnType,
  Location,
  FulfillmentCode,
} = require("@chainlink/functions-toolkit");
const path = require("path");
const process = require("process");

const { networks } = require("../networks");
const addTokenToConfigSecrets = require("./utils/addTokenToConfig");



task(
  "request-weather",
  "Initiates an on-demand request from a Functions Consumer contract"
)
  .addParam("contract", "Address of the consumer contract to call")
  .addParam("subid", "Billing subscription ID used to pay for the request")
  .addParam("secretsref", "encrypted secrets reference")
  .addOptionalParam(
    "simulate",
    "Flag indicating if source JS should be run locally before making an on-chain request",
    true,
    types.boolean
  )
  .addOptionalParam(
    "callbackgaslimit",
    "Maximum amount of gas that can be used to call fulfillRequest in the consumer contract",
    300_000,
    types.int
  )
  .addOptionalParam(
    "slotid",
    "Slot ID to use for uploading DON hosted secrets. If the slot is already in use, the existing encrypted secrets will be overwritten.",
    0,
    types.int
  )
  .addOptionalParam(
    "requestgaslimit",
    "Gas limit for calling the executeRequest function",
    1_500_000,
    types.int
  )
  .setAction(async (taskArgs, hre) => {
    // Get the required parameters
    const contractAddr = taskArgs.contract;
    const subscriptionId = parseInt(taskArgs.subid);
    const slotId = parseInt(taskArgs.slotid);
    const callbackGasLimit = parseInt(taskArgs.callbackgaslimit);

    // Attach to the FunctionsConsumer contract
    const consumerFactory = await ethers.getContractFactory(
      "FunctionsBigQueryConsumer"
    );
    const consumerContract = consumerFactory.attach(contractAddr);

    // Build requestConfig
    let requestConfig = require(path.join(process.cwd(), "/request-config.js"));
    requestConfig = await addTokenToConfigSecrets(requestConfig);

    // Simulate the request
    if (taskArgs.simulate) {
      console.log("\nExecuting Functions Source Script in the local Simulator first...")
      const { responseBytesHexstring, errorString, capturedTerminalOutput } =
        await simulateScript(requestConfig);
      console.log(
        "\n*** console.log output during local simulation: ***\n",
        capturedTerminalOutput || "No console.log output detected.",
        "\n"
      );
      if (responseBytesHexstring) {
        console.log(
          `\nResponse returned by script during local simulation: ${decodeResult(
            responseBytesHexstring,
            requestConfig.expectedReturnType
          )}`
        );
      }
      if (errorString) {
        console.log(`\nError returned by simulated script:\n${errorString}\n`);
      }

      console.log("\nLocal simulation of source code completed...Executing on-chain request...");
    } else {
      console.log("\nSkipping local simulation of the custom source script.")
    }

    // Initialize the subscription manager
    const signer = await ethers.getSigner();
    const linkTokenAddress = networks[network.name]["linkToken"];
    const functionsRouterAddress = networks[network.name]["functionsRouter"];
    const subManager = new SubscriptionManager({
      signer,
      linkTokenAddress,
      functionsRouterAddress,
    });
    await subManager.initialize();

    // Initialize the secrets manager
    const donId = networks[network.name]["donId"];
    const secretsManager = new SecretsManager({
      signer,
      functionsRouterAddress,
      donId,
    });
    await secretsManager.initialize();

    // Validate the consumer contract has been authorized to use the subscription
    const subInfo = await subManager.getSubscriptionInfo(subscriptionId);
    if (
      !subInfo.consumers
        .map(c => c.toLowerCase())
        .includes(contractAddr.toLowerCase())
    ) {
      throw Error(
        `Consumer contract ${contractAddr} has not been added to subscription ${subscriptionId}`
      );
    }

    // Estimate the cost of the request fulfillment
    const { gasPrice } = await hre.ethers.provider.getFeeData();
    const gasPriceWei = BigInt(
      Math.ceil(hre.ethers.utils.formatUnits(gasPrice, "wei").toString())
    );
    const estimatedCostJuels = await subManager.estimateFunctionsRequestCost({
      donId,
      subscriptionId,
      callbackGasLimit,
      gasPriceWei,
    });

    // Ensure that the subscription has a sufficient balance
    const estimatedCostLink = hre.ethers.utils.formatUnits(
      estimatedCostJuels,
      18
    );
    const subBalanceLink = hre.ethers.utils.formatUnits(subInfo.balance, 18);
    if (subInfo.balance <= estimatedCostJuels) {
      throw Error(
        `Subscription ${subscriptionId} does not have sufficient funds. The estimated cost is ${estimatedCostLink} LINK, but the subscription only has ${subBalanceLink} LINK.`
      );
    }

    console.log(
      `\nWaiting for transaction for FunctionsConsumer contract ${contractAddr} on network ${network.name} to be confirmed...`
    );
    // Use a manual gas limit for the request transaction since estimated gas limit is not always accurate
    const overrides = {
      gasLimit: taskArgs.requestgaslimit,
    };
    // If specified, use the gas price from the network config instead of Ethers estimated price
    if (networks[network.name].gasPrice) {
      overrides.gasPrice = networks[network.name].gasPrice;
    }
    // If specified, use the nonce from the network config instead of automatically calculating it
    if (networks[network.name].nonce) {
      overrides.nonce = networks[network.name].nonce;
    }

    let encryptedSecretsReference = taskArgs.secretsref;

    const requestTx = await consumerContract.sendRequest(
      requestConfig.source,
      requestConfig.secretsLocation,
      encryptedSecretsReference,
      requestConfig.args ?? [],
      requestConfig.bytesArgs ?? [],
      subscriptionId,
      callbackGasLimit,
      overrides
    );

    const requestTxReceipt = await requestTx.wait(1);
    const requestId = requestTxReceipt.events[2].args.id;
    console.log(
      `\nTransaction with hash ${requestTx.hash} confirmed. RequestId is ${requestId}`
    );
  });
