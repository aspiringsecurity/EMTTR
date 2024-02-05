import { Box, Flex, Spacer, Text } from "@chakra-ui/react";

import { tradingABI, tradingAddress } from "@/constants/info";

import { useContractRead } from "wagmi";

import {
  addDecimalTwoPlacesFromRight,
  convertUnixEpochToDateString,
  convertToDecimal,
} from "@/helper/functions";

import { useEffect, useState } from "react";

import YesModal from "./YesModal";
import NoModal from "./NoModal";
import Link from "next/link";

export default function BuyCard({ data }) {
  const [dataFetched, setDataFetched] = useState(null);
  const [tokenPrice, setTokenPrice] = useState(0n);
  const [direction, setDirection] = useState("");
  const [marketHandler, setMarketHandler] = useState("");
  const [proxy, setProxy] = useState("");

  const id = data;

  useEffect(() => {
    if (dataFetched) {
      setProxy(dataFetched.proxyAddress);
      setDirection(dataFetched.isAbove ? "Above" : "Below");
      setMarketHandler(dataFetched.marketHandler);
      setTokenPrice(dataFetched.predictionTokenPrice);
    }
  }, [dataFetched]);

  useContractRead({
    address: tradingAddress,
    abi: tradingABI,
    functionName: "getPrediction",
    args: [id],
    onSuccess(data) {
      setDataFetched(data);
    },
  });

  return (
    <>
      <Box
        bgColor="#F3F3F3"
        minW="30%"
        minH="240px"
        float="left"
        m="1.65%"
        borderRadius={10}
        boxShadow="0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19)"
      >
        {dataFetched ? (
          <>
            <Flex direction="column" fontFamily="Barlow" align="left" p={4}>
              <Flex direction="row">
                <Text fontSize={22}>{dataFetched.tokenSymbol.toString()}</Text>
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
              <Flex direction="row">
                <Text>Token Price In USDC : </Text>
                <Text color="#3BC7A6">
                  {addDecimalTwoPlacesFromRight(
                    dataFetched.predictionTokenPrice.toString()
                  )}
                </Text>
              </Flex>
              <Flex>
                <Text>
                  Price Predicted To Be{" "}
                  <Text display="inline" color="#3BC7A6">
                    {direction}
                  </Text>{" "}
                  The Target Price.
                </Text>
              </Flex>
              <Flex mb={4}>
                <Text>Proxy Address : </Text>
                <Link
                  display="inline"
                  target="_blank"
                  href={`https://market.api3.org/dapis?chains=goerli&search=${dataFetched.tokenSymbol.toString()}`}
                >
                  <Text decoration="underline" color="#3BC7A6">
                    {proxy.slice(0, 6) + "..." + proxy.slice(-4)}
                  </Text>
                </Link>{" "}
              </Flex>
              <Text>Are You In Favour Of The Prediction?</Text>
              <Flex direction="row" mt={3}>
                {marketHandler ? (
                  <>
                    <NoModal mhAddress={marketHandler} price={tokenPrice} />
                    <Spacer />
                    <YesModal mhAddress={marketHandler} price={tokenPrice} />
                  </>
                ) : null}
              </Flex>
            </Flex>
          </>
        ) : null}
      </Box>
    </>
  );
}
