const { SecretsManager } = require("@chainlink/functions-toolkit");
const { networks } = require("../networks");
const fs = require("fs");
const path = require("path");

task("upload-to-don", "uploads encrypted secrets to DON")
  .addOptionalParam(
    "slotid",
    "Storage slot number 0 or higher - if the slotid is already in use, the existing secrets for that slotid will be overwritten",
    0,
    types.int
  )
  .addOptionalParam(
    "ttl",
    "Time to live - minutes until the secrets hosted on the DON expire (defaults to 10, and must be at least 5)",
    60,
    types.int
  )
  .setAction(async taskArgs => {
    const signer = await ethers.getSigner();
    const functionsRouterAddress = networks[network.name]["functionsRouter"];
    const donId = networks[network.name]["donId"];
    const gatewayUrls = networks[network.name]["gatewayUrls"]

    const secretsManager = new SecretsManager({
      signer,
      functionsRouterAddress,
      donId,
    });
    await secretsManager.initialize();

    const slotId = parseInt(taskArgs.slotid);
    const minutesUntilExpiration = taskArgs.ttl;

    console.log("Encrypting secrets and uploading to DON...");

    const pathToJsonFile = path.join(
      process.cwd(),
      "/offchain-encrypted-secrets.json"
    );
    const encryptedSecretsObj = JSON.parse(
      fs.readFileSync(pathToJsonFile).toString()
    );

    const {
      version, // Secrets version number (corresponds to timestamp when encrypted secrets were uploaded to DON)
      success, // Boolean value indicating if encrypted secrets were successfully uploaded to all nodes connected to the gateway
    } = await secretsManager.uploadEncryptedSecretsToDON({
      encryptedSecretsHexstring: encryptedSecretsObj.encryptedSecrets,
      gatewayUrls,
      slotId,
      minutesUntilExpiration,
    });


    const encryptedSecretsReference = secretsManager.buildDONHostedEncryptedSecretsReference({
      slotId,
      version,
    })

    console.log(`\nUploaded encrypted secrets to DON at slotId ${slotId} and version ${version}: Status : ${success}.\n*** Please note the following encrypted secrets reference: ${encryptedSecretsReference} for the next step. It will be valid for ${minutesUntilExpiration} minutes ***`)
  });


