import { mhABI } from "@/constants/info";
import { useEffect, useState } from "react";
import { useContractRead } from "wagmi";
import { tradingABI, tradingAddress } from "@/constants/info";

import { useAccount } from "wagmi";

import { Box, Flex, Text, Button, Spacer } from "@chakra-ui/react";
import { addDecimalSixPlacesFromRightAndRemoveTrail } from "@/helper/functions";

import RewardProcess from "./RewardProcess";

export default function RewardCard({ data }) {
  const { address } = useAccount();

  const amountYes = data["amountYes"];
  const amountNo = data["amountNo"];
  const mhAddress = data["marketHandler"];

  const [toShow, setToShow] = useState(false);
  const [rewardsCollected, setRewardsCollected] = useState(null);
  const [winner, setWinner] = useState(null);
  const [predictionId, setPredictionId] = useState(null);
  const [predictionInfo, setPredictionInfo] = useState(null);
  const [startClaim, setStartClaim] = useState(false);

  useContractRead({
    address: mhAddress,
    abi: mhABI,
    functionName: "rewardCollected",
    args: [address],
    onSuccess(data) {
      setRewardsCollected(data);
    },
  });

  useContractRead({
    address: mhAddress,
    abi: mhABI,
    functionName: "winner",
    onSuccess(data) {
      setWinner(data);
    },
  });

  useContractRead({
    address: mhAddress,
    abi: mhABI,
    functionName: "I_SELF_ID",
    onSuccess(data) {
      setPredictionId(data);
    },
  });

  useEffect(() => {
    if (predictionId) {
      readPredictionData.refetch();
    }
  }, [predictionId]);

  const readPredictionData = useContractRead({
    address: tradingAddress,
    abi: tradingABI,
    functionName: "getPrediction",
    args: [predictionId ? predictionId : 0],
    onSuccess(data) {
      setPredictionInfo(data);
    },
  });

  useEffect(() => {
    if (rewardsCollected != null && rewardsCollected == false) {
      if (winner == true) {
        if (amountYes > 0n) {
          setToShow(true);
        }
      } else {
        if (amountNo > 0n) {
          setToShow(true);
        }
      }
    }
  }, [rewardsCollected, winner]);

  return (
    <>
      {toShow && (winner || !winner) ? (
        <>
          <Box
            bgColor="#F3F3F3"
            minH="150px"
            minW="200px"
            maxW="fit-content"
            float="left"
            m="1.65%"
            borderRadius={10}
            p={5}
            fontFamily="Barlow"
            boxShadow="0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19)"
          >
            {predictionInfo && (
              <>
                <Flex direction="row">
                  <Text fontSize={22}>{predictionInfo.tokenSymbol}</Text>
                  <Spacer />
                  {predictionId && (
                    <Text color="#3BC7A6"> {predictionId.toString()}</Text>
                  )}
                </Flex>
                <Flex direction="row" gap={2}>
                  <Text>Tokens held at winning side : </Text>
                  <Text color="#3BC7A6">
                    {winner == true
                      ? addDecimalSixPlacesFromRightAndRemoveTrail(
                          amountYes.toString()
                        )
                      : addDecimalSixPlacesFromRightAndRemoveTrail(
                          amountNo.toString()
                        )}
                  </Text>
                </Flex>
                <Button
                  colorScheme="green"
                  mt={5}
                  onClick={() => setStartClaim(true)}
                >
                  Collect
                </Button>
              </>
            )}
          </Box>
        </>
      ) : null}
      {startClaim ? <RewardProcess mhAddress={mhAddress} /> : null}
    </>
  );
}
