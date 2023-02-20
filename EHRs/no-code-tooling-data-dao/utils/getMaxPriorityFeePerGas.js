export async function getMaxPriorityFeePerGas(provider) {
  // Blame FEVM
  let maxPriorityFee = null;
  let attempt = 0;
  while (maxPriorityFee == null) {
    try {
      return await provider.getFeeData().maxPriorityFeePerGas;
    } catch (e) {
      attempt++;
      if (attempt > 100) {
        break;
      }
    }
  }
  return 0;
}
