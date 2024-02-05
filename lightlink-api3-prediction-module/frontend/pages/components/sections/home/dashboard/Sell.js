import {
  usePrepareContractWrite,
  useContractWrite,
  useWaitForTransaction,
} from "wagmi";

import { mhABI } from "@/constants/info";

import { toast } from "react-toastify";

import { useState, useEffect } from "react";

export default function Sell({ setStartReset, handler, tokenType, amount }) {
  const [sold, setSold] = useState(false);
  const [functionPrepared, setFunctionPrepared] = useState(false);

  const { config: sellConfig } = usePrepareContractWrite({
    address: handler,
    abi: mhABI,
    functionName: tokenType == "yes" ? "sellYesToken" : "sellNoToken",
    args: [amount],
    onSuccess() {
      setFunctionPrepared(true);
    },
  });
  const { data, write: sellWrite } = useContractWrite(sellConfig);

  const sellWait = useWaitForTransaction({
    hash: data?.hash,
    onSuccess() {
      setSold(true);
    },
  });

  useEffect(() => {
    if (sold) {
      toast.success("Successfully Sold!");
      setSold(false);
      setStartReset(true);
    }
  }, [sold]);

  useEffect(() => {
    if (functionPrepared) sellWrite();
  }, [functionPrepared]);
}
