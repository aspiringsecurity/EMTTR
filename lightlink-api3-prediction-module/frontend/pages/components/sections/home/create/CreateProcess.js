import { useContractWrite, useWaitForTransaction } from "wagmi";

import { toast } from "react-toastify";

import { useEffect, useState } from "react";

import { tradingABI, tradingAddress } from "@/constants/info";

import { AbiCoder } from "ethers";

export default function CreateProcess({
  tokenType,
  proxyAddress,
  isAbove,
  targetPrice,
  dueDate,
  basePrice,
  setStartReset,
}) {
  const abiHandler = AbiCoder.defaultAbiCoder();
  const editedIsabove = abiHandler.encode(
    ["bool"],
    [isAbove == "false" ? false : true]
  );

  const [createdPrediction, setCreatedPrediction] = useState(false);

  const { data, write: createPredictionWrite } = useContractWrite({
    address: tradingAddress,
    abi: tradingABI,
    functionName: "createPrediction",
    args: [
      tokenType,
      proxyAddress,
      editedIsabove,
      targetPrice,
      dueDate,
      basePrice,
    ],
  });

  const waitCreatePrediction = useWaitForTransaction({
    hash: data?.hash,
    onSuccess() {
      setCreatedPrediction(true);
    },
  });

  useEffect(() => {
    if (createdPrediction) {
      toast.success(
        "Market Created Successfully! Can be viewed in the `Buy` section shortly."
      );
      setCreatedPrediction(false);
      setStartReset(true);
    }
  }, [createdPrediction]);

  useEffect(() => {
    try {
      createPredictionWrite();
    } catch (err) {
      console.log("Oops, creation failed.");
    }
  }, []);
}
