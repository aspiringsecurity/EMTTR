import {
  FormControl,
  FormLabel,
  Input,
  Select,
  Box,
  FormErrorMessage,
  Button,
  Text,
} from "@chakra-ui/react";

import { usdcAddress, usdcABI, tradingAddress } from "@/constants/info";

import {
  usePrepareContractWrite,
  useContractWrite,
  useWaitForTransaction,
  useContractRead,
  useAccount,
} from "wagmi";

import CreateProcess from "./CreateProcess";

import { useEffect, useState } from "react";

import { toast } from "react-toastify";

import Link from "next/link";

import { encodeBytes32String, parseUnits } from "ethers";

export default function CreateGetInformation() {
  const { address } = useAccount();

  const [firstRun, setFirstRun] = useState(true);

  const [startReset, setStartReset] = useState(false);
  const [approved, setApproved] = useState(false);
  const [currentApprovedAmount, setCurrentApprovedAmount] = useState(0);
  const [startOperation, setStartOperation] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);

  const [error, setError] = useState("");

  const [tokenType, setTokenType] = useState("");
  const [proxy, setProxy] = useState("");
  const [dueDate, setDueDate] = useState(0);
  const [basePrice, setBasePrice] = useState(100);
  const [targetPrice, setTargetPrice] = useState(0);
  const [displayTargetPrice, setDisplayTargetPrice] = useState(100);
  const [isAbove, setIsAbove] = useState(true);

  function resetVariables() {
    setIsSubmitting(false);
    setTokenType("");
    setProxy("");
    setDueDate(0);
    setBasePrice(100);
    setTargetPrice(0);
    setDisplayTargetPrice(100);
    setIsAbove(true);
    setCurrentApprovedAmount(0);
    setApproved(false);
    setStartOperation(false);
    setStartReset(false);
  }

  // APPROVAL ====================

  const usdcRead = useContractRead({
    address: usdcAddress,
    abi: usdcABI,
    functionName: "allowance",
    args: [address, tradingAddress],
    onSuccess(data) {
      setCurrentApprovedAmount(data);
    },
  });

  const { config } = usePrepareContractWrite({
    address: usdcAddress,
    abi: usdcABI,
    functionName: "approve",
    args: [tradingAddress, 50000000n],
  });
  const { data, write: usdcApprovalWrite } = useContractWrite(config);

  const waitObj = useWaitForTransaction({
    hash: data?.hash,
    onSuccess() {
      console.log("Success Approval");
      setApproved(true);
    },
  });

  // =====================

  const handleSubmit = (e) => {
    e.preventDefault();

    setIsSubmitting(true);
    // validate form data
    if (
      !dueDate ||
      !tokenType ||
      !displayTargetPrice ||
      !isAbove ||
      !basePrice
    ) {
      setError(
        "Please fill      setHasValueChanged(true); out all required fields."
      );
      setIsSubmitting(false);
      return;
    }

    if (basePrice < 1) {
      setError("Please set the base price >= 1.");
      setIsSubmitting(false);
      return;
    }

    const dueDateObject = new Date(dueDate);
    const currentDateObject = new Date();
    if (dueDateObject <= currentDateObject) {
      setError("Due date must be in the future.");
      setIsSubmitting(false);
      return;
    }
    const unixEpochTime = Math.floor(dueDateObject.getTime() / 1000).toString();
    setDueDate(unixEpochTime);

    const validatedValue = parseFloat(displayTargetPrice).toFixed(5);
    const ethValue = parseUnits(validatedValue.toString(), "ether");
    const weiValue = ethValue.toString();
    setTargetPrice(weiValue);

    setError("");

    setStartOperation(true);
  };

  useEffect(() => {
    if (startReset) {
      resetVariables();
    }
  }, [startReset]);

  useEffect(() => {
    if (approved) {
      toast.info("Please wait while we proceed further...");
    }
  }, [approved]);

  useEffect(() => {
    if (startOperation) {
      if (currentApprovedAmount == 0) usdcApprovalWrite();
      else setApproved(true);
    }
  }, [startOperation, currentApprovedAmount]);

  return (
    <>
      <Box align="center" justify="center" pt={5} mb={100}>
        <Box w="50%" border="1px solid gray" borderRadius={10} p={10}>
          <form onSubmit={handleSubmit}>
            <FormControl isRequired isInvalid={error}>
              <FormLabel>Asset To Predict</FormLabel>
              <Input
                type="text"
                placeholder="AAPL"
                disabled={isSubmitting}
                value={tokenType}
                onChange={(e) => setTokenType(e.target.value.toString())}
              />
              <FormLabel mt={5}>
                Proxy Address{" "}
                <Link
                  href="https://market.api3.org/dapis?chains=goerli"
                  target="_blank"
                >
                  (
                  <Text color="blue" display="inline" textDecor="underline">
                    https://market.api3.org/dapis
                  </Text>
                  )
                </Link>
              </FormLabel>
              <Input
                type="text"
                placeholder="0xa34Aa31eb0d9c4414EaC3B4A4b56A7ca3752529d"
                disabled={isSubmitting}
                value={proxy}
                onChange={(e) => setProxy(e.target.value)}
                onBlur={() => {
                  if (firstRun) {
                    toast.info(
                      "Please make sure the Proxy Address is correct or else it will lead to ambiguity."
                    );
                    setFirstRun(false);
                  }
                }}
              />
              <FormLabel mt={5}>Deadline</FormLabel>
              <Input
                disabled={isSubmitting}
                type="date"
                value={dueDate}
                onChange={(e) => setDueDate(e.target.value)}
              />
              <FormLabel mt={5}>Target Price In USD</FormLabel>
              <Input
                type="number"
                placeholder="1000.00000"
                step="0.00001"
                disabled={isSubmitting}
                value={displayTargetPrice}
                onChange={(e) => setDisplayTargetPrice(e.target.value)}
              />{" "}
              <FormLabel mt={5}>Will Be Above The Target?</FormLabel>
              <Select
                disabled={isSubmitting}
                value={isAbove}
                onChange={(e) => setIsAbove(e.target.value)}
              >
                <option value="true">True</option>
                <option value="false">False</option>
              </Select>
              <FormLabel mt={5}>Cost Of A Tradable Token In Cents</FormLabel>
              <Input
                type="number"
                placeholder="100"
                disabled={isSubmitting}
                value={basePrice}
                onChange={(e) => setBasePrice(e.target.value)}
              />
              <Button colorScheme="green" onClick={handleSubmit} mt={5}>
                Create
              </Button>
              <FormErrorMessage>{error}</FormErrorMessage>
            </FormControl>
          </form>
        </Box>
      </Box>
      {approved && (
        <CreateProcess
          tokenType={encodeBytes32String(tokenType)}
          proxyAddress={proxy}
          isAbove={isAbove}
          targetPrice={targetPrice}
          dueDate={dueDate}
          basePrice={basePrice}
          setStartReset={setStartReset}
        />
      )}
    </>
  );
}
