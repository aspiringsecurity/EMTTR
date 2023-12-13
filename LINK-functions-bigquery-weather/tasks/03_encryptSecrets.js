const { SecretsManager } = require("@chainlink/functions-toolkit");
const { networks } = require("../networks");
const fs = require("fs");
const path = require("path");
const process = require("process");

const addTokenToConfigSecrets = require("./utils/addTokenToConfig");

task(
  "encrypt-secrets",
  "encrypts secrets needed for a Functions request execution"
)
  .addOptionalParam(
    "output",
    "Output JSON file name (defaults to offchain-encrypted-secrets.json)",
    "offchain-encrypted-secrets.json",
    types.string
  )
  .setAction(async taskArgs => {
    const signer = await ethers.getSigner();
    const functionsRouterAddress = networks[network.name]["functionsRouter"];
    const donId = networks[network.name]["donId"];

    const secretsManager = new SecretsManager({
      signer,
      functionsRouterAddress,
      donId,
    });
    await secretsManager.initialize();

    // Get the secrets object from  Functions-request-config.js or other specific request config.
    let requestConfig = require(path.join(
      process.cwd(),
      "/request-config.js"
    ));

    if (!requestConfig.secrets || requestConfig.secrets.length === 0) {
      console.log("No secrets found in the request config.");
      return;
    }

    // Generate JWT token and add it to Request Config's `secrets` property.
    requestConfig = await addTokenToConfigSecrets(requestConfig);

    const outputfile = path.join(process.cwd(), taskArgs.output);
    console.log(
      `\nEncrypting secrets and writing to JSON file '${outputfile}'...`
    );

    const encryptedSecretsObj = await secretsManager.encryptSecrets(
      requestConfig.secrets
    );
    fs.writeFileSync(outputfile, JSON.stringify(encryptedSecretsObj));

    console.log(`\nWrote offchain secrets file to '${outputfile}'.`);

  });
