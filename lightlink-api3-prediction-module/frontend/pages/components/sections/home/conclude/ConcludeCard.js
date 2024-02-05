import { Box, Flex, Spacer, Text, Button } from "@chakra-ui/react";

import {
  usePrepareContractWrite,
  useContractWrite,
  useWaitForTransaction,
} from "wagmi";

import {
  tradingABI,
  tradingAddress,
  settlementABI,
  settlementAddress,
} from "@/constants/info";

import { useContractRead } from "wagmi";

import {
  convertUnixEpochToDateString,
  convertToDecimal,
} from "@/helper/functions";

import { useState } from "react";

export default function ConcludeCard({ id }) {
  console.log(id);

  const [dataFetched, setDataFetched] = useState(null);

  const [functionPrepared, setFunctionPrepared] = useState(false);
  const [concluded, setConcluded] = useState(false);

  /// ==================

  useContractRead({
    address: tradingAddress,
    abi: tradingABI,
    functionName: "getPrediction",
    args: [id],
    onSuccess(data) {
      setDataFetched(data);
    },
  });

  /// ===========
  const { config: concludeConfig } = usePrepareContractWrite({
    address: settlementAddress,
    abi: settlementABI,
    functionName: "concludePrediction_1",
    args: [id],
    onSuccess() {
      setFunctionPrepared(true);
    },
  });

  const { data, write: concludeWrite } = useContractWrite(concludeConfig);

  const sellWait = useWaitForTransaction({
    hash: data?.hash,
    onSuccess() {
      setConcluded(true);
    },
  });

  return (
    <>
      {!concluded ? (
        <>
          {" "}
          <Box
            bgColor="#F3F3F3"
            minW="20%"
            minH="fit-content"
            float="left"
            m="1.65%"
            borderRadius={10}
            boxShadow="0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19)"
          >
            {dataFetched && functionPrepared ? (
              <>
                <Flex direction="column" fontFamily="Barlow" align="left" p={4}>
                  <Flex direction="row">
                    <Text fontSize={22}>
                      {dataFetched.tokenSymbol.toString()}
                    </Text>
                    <Spacer />
                    <Text color="#3BC7A6">{id}</Text>
                  </Flex>{" "}
                  <Flex direction="row">
                    <Text>Deadline : </Text>
                    <Text color="#3BC7A6">
                      {"  "}
                      {convertUnixEpochToDateString(dataFetched.deadline)}
                    </Text>
                  </Flex>
                  <Flex direction="row">
                    <Text>Target Price : </Text>
                    <Text color="#3BC7A6">
                      {"  "}
                      {convertToDecimal(dataFetched.targetPricePoint)}
                    </Text>
                  </Flex>
                  <Button
                    colorScheme="green"
                    onClick={() => concludeWrite()}
                    mt={5}
                  >
                    CONCLUDE
                  </Button>
                </Flex>
              </>
            ) : null}
          </Box>
        </>
      ) : null}
    </>
  );
}
