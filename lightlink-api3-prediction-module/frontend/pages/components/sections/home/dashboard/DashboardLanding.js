import { useEffect, useState } from "react";
import { Flex, Box, Heading } from "@chakra-ui/react";
import DashboardCard from "./DashboardCard";

import "@fontsource/vt323";

export default function DashboardLanding({ data }) {
  const [keys, setKeys] = useState([]);
  const [tokensOwnedForPredictions, setTokensOwnedForPredictions] = useState(
    {}
  );
  const [cardObjects, setCardObjects] = useState([]);

  useEffect(() => {
    setKeys(Object.keys(data));
  }, []);

  useEffect(() => {
    if (keys.length > 0) {
      var workingObj = {};

      keys.forEach((e) => {
        const currentObj = data[e];
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
      setTokensOwnedForPredictions(workingObj);
    }
  }, [keys]);

  useEffect(() => {
    if (Object.keys(tokensOwnedForPredictions).length > 0) {
      setCardObjects(Object.entries(tokensOwnedForPredictions));
    }
  }, [tokensOwnedForPredictions]);

  return (
    <>
      <Box m={10} maxH="max-content">
        <Flex direction="row" ml={5}>
          <Heading fontFamily="vt323" size="4xl" mb={5}>
            Your Investments
          </Heading>
        </Flex>
        {cardObjects
          ? cardObjects.map((item, index) => (
              <DashboardCard key={index} data={item} />
            ))
          : null}
      </Box>
    </>
  );
}
