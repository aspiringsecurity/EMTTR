import {
  usePrepareContractWrite,
  useContractWrite,
  useWaitForTransaction,
} from "wagmi";

import { mhABI } from "@/constants/info";

import { toast } from "react-toastify";

import { useEffect, useState } from "react";

export default function RewardProcess({ mhAddress }) {
  const [functionPrepared, setFunctionPrepared] = useState(false);
  const [claimed, setClaimed] = useState(false);

  const { config: claimConfig } = usePrepareContractWrite({
    address: mhAddress,
    abi: mhABI,
    functionName: "collectRewards",
    onSuccess() {
      setFunctionPrepared(true);
    },
  });
  const { data: claimData, write: claimWrite } = useContractWrite(claimConfig);

  const claimWait = useWaitForTransaction({
    hash: claimData?.hash,
    onSuccess() {
      setClaimed(true);
    },
  });

  useEffect(() => {
    if (claimed) {
      toast.success("Successfully Claimed!");
      setClaimed(false);
    }
  }, [claimed]);

  useEffect(() => {
    if (functionPrepared) claimWrite();
  }, [functionPrepared]);
}
