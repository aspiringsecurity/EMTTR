async function main() {
  const addr = process.env.CONTRACT_ADDRESS;
  const delayInsurance = await hre.ethers.getContractAt("DelayInsurance", addr);

  // Create insurance policy
  const shipId = "b8625b67-7142-cfd1-7b85-595cebfe4191";
  await delayInsurance.subscribePolicy(
    shipId,
    100,
    974448412,
    1763366812,
    1000,
    200000
  )
  console.log("Creating policy contract for boat id : " + shipId );

  

  console.log(await delayInsurance.getAllPolicies());
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
