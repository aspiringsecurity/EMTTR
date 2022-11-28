async function main() {
  const addr = process.env.CONTRACT_ADDRESS;
  const delayInsurance = await hre.ethers.getContractAt("DelayInsurance", addr);

  /* Initialise protocols functions */

  //TO DO : UPGRADE NONCE MANNUALY TO SEND ALL TRANSACTIONS SUCCESFULLY.

  // Set weather oracle datas
  const benefAddr = "0x56A617E336045C9be6653f8ACEeCcE4D32c57cE2";
  await delayInsurance.payOut(benefAddr);
  console.log("Pay out down to " + benefAddr);

}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
