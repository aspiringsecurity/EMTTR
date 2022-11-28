async function main() {
  const addr = process.env.CONTRACT_ADDRESS;
  const delayInsurance = await hre.ethers.getContractAt("DelayInsurance", addr);

  // Set policy threshold
  const policyAddr = "";
  const gustThreshold = 5;
  await delayInsurance.setPolicyThreshold(policyAddr, gustThreshold);
  console.log("Policy gust threshold set to : " + gustThreshold);

}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
