const { types } = require("hardhat/config");
const { networks } = require("../networks");

task("deploy-consumer", "Deploys the FunctionsBigQueryConsumer contract")
  .addOptionalParam(
    "verify",
    "Set to true to verify contract",
    false,
    types.boolean
  )
  .setAction(async taskArgs => {
    console.log(`Deploying FunctionsConsumer contract to ${network.name}`);

    const functionsRouter = networks[network.name]["functionsRouter"];
    const donIdBytes32 = hre.ethers.utils.formatBytes32String(
      networks[network.name]["donId"]
    );

    console.log("\n__Compiling Contracts__");
    await run("compile");

    const overrides = {};
    // If specified, use the gas price from the network config instead of Ethers estimated price
    if (networks[network.name].gasPrice) {
      overrides.gasPrice = networks[network.name].gasPrice;
    }

    const consumerContractFactory = await ethers.getContractFactory(
      "FunctionsBigQueryConsumer"
    );
    const consumerContract = await consumerContractFactory.deploy(
      functionsRouter,
      donIdBytes32,
      overrides
    );

    console.log(
      `\nWaiting ${
        networks[network.name].confirmations
      } blocks for transaction ${
        consumerContract.deployTransaction.hash
      } to be confirmed...`
    );
    await consumerContract.deployTransaction.wait(
      networks[network.name].confirmations
    );

    console.log(
      "\nDeployed FunctionsConsumer contract to:",
      consumerContract.address
    );

    const verifyContract = taskArgs.verify;
    if (
      verifyContract &&
      !!networks[network.name].verifyApiKey &&
      networks[network.name].verifyApiKey !== "UNSET"
    ) {
      try {
        console.log("\nVerifying contract...");
        await consumerContract.deployTransaction.wait(
          Math.max(4 - networks[network.name].confirmations, 0)
        );
        await run("verify:verify", {
          address: consumerContract.address,
          constructorArguments: [functionsRouter, donIdBytes32],
        });
        console.log("Contract verified");
      } catch (error) {
        if (!error.message.includes("Already Verified")) {
          console.log(
            "Error verifying contract.  Delete the build folder and try again."
          );
          console.log(error);
        } else {
          console.log("Contract already verified");
        }
      }
    } else if (verifyContract) {
      console.log(
        "\nPOLYGONSCAN_API_KEY, ETHERSCAN_API_KEY or FUJI_SNOWTRACE_API_KEY is missing. Skipping contract verification..."
      );
    }

    console.log(
      `\nFunctionsConsumer contract deployed to ${consumerContract.address} on ${network.name}`
    );
  });
