import { useEffect, useState } from "react";
import { ApolloClient, InMemoryCache, gql } from "@apollo/client";

import { graphEndpoint } from "@/constants/info";

import { toast } from "react-toastify";

import BuyLanding from "./BuyLanding";
import { Flex, Heading } from "@chakra-ui/react";
import { FadeInWhenVisible } from "@/pages/components/TransitionBoxes";

export default function BuyGetInformation() {
  const [dataFetched, setDataFetched] = useState(null);
  const [length, setLength] = useState(0);
  const [ids, setIds] = useState(null);
  const [graphDataFetched, setGraphDataFetched] = useState(null);

  const [displayNone, setDisplayNone] = useState(false);

  const currentTime = Date.now();
  const unixEpoch = Math.floor(currentTime / 1000);
  const queryFinal = `query AvailableMarkets\n{\n  predictionCreateds(orderBy: deadline, where: {deadline_gt: ${unixEpoch}}) {\n  marketHandler\n  predictionId\n  }\n}`;

  const client = new ApolloClient({
    uri: graphEndpoint,
    cache: new InMemoryCache(),
  });

  useEffect(() => {
    if (length > 0) {
      setDataFetched(true);
    }
  }, [length]);

  useEffect(() => {
    if (ids && ids.length >= 1) {
      setLength(ids.length);
    }
  }, [ids]);

  useEffect(() => {
    var idsArray = [];
    if (graphDataFetched) {
      for (let key in graphDataFetched) {
        idsArray.push(graphDataFetched[key]["predictionId"]);
      }
    }
    if (typeof idsArray != null) {
      setIds(idsArray);
    }
  }, [graphDataFetched]);

  useEffect(() => {
    async function execute() {
      var { data } = await client.query({
        query: gql`
          ${queryFinal}
        `,
      });
      const finalArray = data.predictionCreateds;
      if (finalArray.length == 0) {
        setDisplayNone(true);
      }
      setGraphDataFetched(finalArray);
    }
    toast.info("Please Wait...");
    execute();
  }, []);

  return (
    <>
      {displayNone ? (
        <>
          <FadeInWhenVisible>
            <Flex
              direction="column"
              align="center"
              justify="center"
              h={500}
              w="70%"
            >
              <Heading fontFamily="Barlow" fontSize="90px">
                It looks like there are no available markets :(
              </Heading>
            </Flex>
          </FadeInWhenVisible>
        </>
      ) : dataFetched && ids && length ? (
        <BuyLanding ids={ids} length={length} />
      ) : null}
    </>
  );
}
