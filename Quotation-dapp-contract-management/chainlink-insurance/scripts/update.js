async function main() {
  const addr = process.env.CONTRACT_ADDRESS;
  const delayInsurance = await hre.ethers.getContractAt("DelayInsurance", addr);

  // Set weather oracle datas
  await delayInsurance.UpdateContracts();
  console.log("Updating contracts");

}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
