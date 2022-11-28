async function main() {
  const addr = process.env.CONTRACT_ADDRESS;
  const delayInsurance = await hre.ethers.getContractAt("DelayInsurance", addr);

  /* Initialise protocols functions */

  // Set weather oracle datas
  const oracleAddr = process.env.ORACLE_ADDRESS;
  const jobId = process.env.JOB_ID;
  const fee = process.env.JOB_FEE;
  await delayInsurance.setWeatherOracle(oracleAddr, jobId, fee);
  console.log("Oracle address set to : " + oracleAddr);
  console.log("Job Id set to : " + jobId);
  console.log("Fees set to : " + fee);

  // // Set the incident threshold
  // const incidentsThreshold = 1
  // await delayInsurance.setIncidentThreshold(incidentsThreshold);
  // console.log("Incident threshold set to : " + incidentsThreshold);
  //
  // // Set update timer in seconds (1 hour = 60 * 60)
  // const updateTimer = 60;
  // await delayInsurance.setUpdateTimer(updateTimer);
  // console.log("Keeper update timer set to : " + updateTimer);

}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
