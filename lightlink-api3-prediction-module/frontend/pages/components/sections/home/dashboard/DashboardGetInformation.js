import { useAccount } from "wagmi";
import { useEffect, useState } from "react";
import { ApolloClient, InMemoryCache, gql } from "@apollo/client";
import { graphEndpoint } from "@/constants/info";

import { Flex, Heading } from "@chakra-ui/react";

import { toast } from "react-toastify";

import DashboardLanding from "./DashboardLanding";
import { FadeInWhenVisible } from "@/pages/components/TransitionBoxes";

export default function DashboardGetInformation() {
  const { address } = useAccount();

  const [dataFetched, setDataFetched] = useState(null);
  const [displayNone, setDisplayNone] = useState(false);

  const queryFinal = `query AccountInformation\n{\n  handlerProgresses(\n    orderBy: predictionId\n    where: {trader: "${address}"}\n    orderDirection: asc\n  ) {\n    predictionId\n    marketHandler\n    trader\n    amountNo\n    amountYes\n  }\n}`;

  const client = new ApolloClient({
    uri: graphEndpoint,
    cache: new InMemoryCache(),
  });

  useEffect(() => {
    async function execute() {
      var { data } = await client.query({
        query: gql`
          ${queryFinal}
        `,
      });
      const finalArray = data.handlerProgresses;
      if (finalArray.length == 0) {
        setDisplayNone(true);
      }
      setDataFetched(finalArray);
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
              h={600}
              w="80%"
            >
              <Heading fontFamily="Barlow" fontSize="70px">
                It looks like you have no investments available. Please head
                back to the 'Buy' section or wait a while for the changes to
                reflect.
              </Heading>
            </Flex>
          </FadeInWhenVisible>
        </>
      ) : dataFetched ? (
        <DashboardLanding data={dataFetched} />
      ) : null}
    </>
  );
}
