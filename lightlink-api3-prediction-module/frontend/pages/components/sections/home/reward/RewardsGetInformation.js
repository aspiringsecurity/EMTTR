import { useState, useEffect } from "react";

import { ApolloClient, InMemoryCache, gql } from "@apollo/client";
import { graphEndpoint } from "@/constants/info";

import { Flex, Heading } from "@chakra-ui/react";
import { FadeInWhenVisible } from "@/pages/components/TransitionBoxes";

import { useAccount } from "wagmi";

import { toast } from "react-toastify";

import RewardsLanding from "./RewardsLanding";

export default function RewardsGetInformation() {
  const { address } = useAccount();

  const client = new ApolloClient({
    uri: graphEndpoint,
    cache: new InMemoryCache(),
  });

  const [displayNone, setDisplayNone] = useState(false);

  const [concludedPredictions, setConcludedPredictions] = useState(null);

  const [userDataFetched, setUserDataFetched] = useState(null);
  const [keys, setKeys] = useState([]);
  const [configuredUserData, setConfiguredUserData] = useState(null);

  const concludedPredictionsQuery =
    "{\n\tpredictionConcludeds(orderBy: predictionId) {\n\tpredictionId\n}\n\t}";
  const userDataQuery = `query AccountInformation\n{\n  handlerProgresses(\n    orderBy: predictionId\n    where: {trader: "${address}"}\n    orderDirection: asc\n  ) {\n    predictionId\n    marketHandler\n    trader\n    amountNo\n    amountYes\n  }\n}`;

  async function getArray(arr) {
    const final = [];
    for (var index = 0; index < arr.length; index++) {
      final.push(arr[index]["predictionId"]);
    }

    return final;
  }

  useEffect(() => {
    async function execute() {
      var { data: concludeData } = await client.query({
        query: gql`
          ${concludedPredictionsQuery}
        `,
      });
      const concludeArray = await getArray(concludeData.predictionConcludeds);
      setConcludedPredictions(concludeArray);

      /// =========

      var { data: userData } = await client.query({
        query: gql`
          ${userDataQuery}
        `,
      });
      const finalArray = userData.handlerProgresses;
      if (finalArray.length == 0) {
        setDisplayNone(true);
      }
      setUserDataFetched(finalArray);
    }

    toast.info("Please Wait...");
    execute();
  }, []);

  useEffect(() => {
    if (userDataFetched) setKeys(Object.keys(userDataFetched));
  }, [userDataFetched]);

  useEffect(() => {
    if (keys.length > 0) {
      var workingObj = {};

      keys.forEach((e) => {
        const currentObj = userDataFetched[e];
        const predictionId = currentObj.predictionId;
        const marketHandler = currentObj.marketHandler;

        const amountNo = BigInt(currentObj.amountNo);
        const amountYes = BigInt(currentObj.amountYes);

        if (!workingObj.hasOwnProperty(predictionId)) {
          const updatedObject = {
            ...workingObj,
            [predictionId]: {
              ["amountNo"]: amountNo,
              ["amountYes"]: amountYes,
              marketHandler: marketHandler,
            },
          };
          workingObj = updatedObject;
        } else {
          const finalNo = workingObj[predictionId]["amountNo"] + amountNo;
          const finalYes = workingObj[predictionId]["amountYes"] + amountYes;

          const updatedObject = {
            ...workingObj,
            [predictionId]: {
              ["amountNo"]: finalNo,
              ["amountYes"]: finalYes,
              marketHandler: marketHandler,
            },
          };
          workingObj = updatedObject;
        }
      });
      setConfiguredUserData(workingObj);
    }
  }, [keys]);

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
                It looks like you have no rewards awaiting just yet. Check back
                later :)
              </Heading>
            </Flex>
          </FadeInWhenVisible>
        </>
      ) : (
        <>
          {configuredUserData && concludedPredictions ? (
            <>
              <RewardsLanding
                userData={configuredUserData}
                concluded={concludedPredictions}
              />
            </>
          ) : null}
        </>
      )}
    </>
  );
}
