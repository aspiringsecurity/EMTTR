import {
  usePrepareContractWrite,
  useContractWrite,
  useWaitForTransaction,
} from "wagmi";

import { mhABI } from "@/constants/info";

import { toast } from "react-toastify";

import { useState, useEffect } from "react";

export default function Swap({ setStartReset, handler, finalType, amount }) {
  const [swapped, setSwapped] = useState(false);
  const [functionPrepared, setFunctionPrepared] = useState(false);

  const { config: swapConfig } = usePrepareContractWrite({
    address: handler,
    abi: mhABI,
    functionName:
      finalType == "yes" ? "swapTokenNoWithYes" : "swapTokenYesWithNo",
    args: [amount],
    onSuccess() {
      setFunctionPrepared(true);
    },
  });
  const { data: swapData, write: swapWrite } = useContractWrite(swapConfig);

  const swapWait = useWaitForTransaction({
    hash: swapData?.hash,
    onSuccess() {
      setSwapped(true);
    },
  });

  useEffect(() => {
    if (swapped) {
      toast.success("Successfully Sold!");
      setSwapped(false);
      setStartReset(true);
    }
  }, [swapped]);

  useEffect(() => {
    if (functionPrepared) swapWrite();
  }, [functionPrepared]);
}
