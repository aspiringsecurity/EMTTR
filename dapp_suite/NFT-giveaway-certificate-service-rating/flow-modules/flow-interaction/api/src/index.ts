import * as fcl from "@onflow/fcl";
import fs from "fs";
import { hideBin } from "yargs/helpers";
import yargs from "yargs/yargs";
import initApp from "./app";
import { getConfig } from "./config";
import initDB from "./db";
import { TemplateService } from "./services/template";
import * as cron from "cron";

const argv = yargs(hideBin(process.argv)).argv;
const DEV = argv.dev;

let envVars;

if (DEV) {
  const env = require("dotenv");
  const expandEnv = require("dotenv-expand");

  const config = env.config({
    path: process.cwd() + "/.env.local",
  });

  expandEnv(config);
  console.log("CONFIG", config);
  envVars = config.parsed;
}

async function run() {
  const config = getConfig(envVars);
  const db = initDB(config);

  // Make sure to disconnect from DB when exiting the process
  process.on("SIGTERM", () => {
    db.destroy().then(() => {
      process.exit(0);
    });
  });

  try {
    fs.unlinkSync(config.dbPath);
  } catch (e) {}

  // Run all database migrations
  await db.migrate.latest();

  // Make sure we're pointing to the correct Flow Access API.
  fcl.config().put("accessNode.api", config.accessApi);

  const startAPIServer = async () => {
    console.log("Starting API server ....");
    const templateService = new TemplateService(config);

    console.log("...Seeding TemplateService...");
    await templateService.seed();
    console.log("Seeded TemplateService!");

    const CronJob = cron.CronJob;
    const job = new CronJob(
      "*/5 * * * *",
      async function () {
        console.log("...Syncing TemplateService...");
        await templateService.seed();
        console.log("Synched TemplateService!");
      },
      null,
      true,
      "America/Los_Angeles"
    );

    const auditorsJSONFile = config.auditorsJsonFile
      ? JSON.parse(fs.readFileSync(config.auditorsJsonFile, "utf8"))
      : {};

    const namesJSONFile = config.namesJsonFile
      ? JSON.parse(fs.readFileSync(config.namesJsonFile, "utf8"))
      : {};

    const app = initApp(templateService, auditorsJSONFile, namesJSONFile);

    app.listen(config.port, () => {
      console.log(`Listening on port ${config.port}!`);
    });
  };

  await startAPIServer();
}

const redOutput = "\x1b[31m%s\x1b[0m";

run().catch((e) => {
  console.error(redOutput, e);
  process.exit(1);
});
