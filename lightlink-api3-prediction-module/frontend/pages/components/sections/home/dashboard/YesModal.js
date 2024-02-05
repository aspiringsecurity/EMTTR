import { addDecimalSixPlacesFromRightAndRemoveTrail } from "@/helper/functions";
import { Button, Text, Flex } from "@chakra-ui/react";

import { useEffect, useState } from "react";

import Sell from "./Sell";
import Swap from "./Swap";

export default function YesModal({ data }) {
  const [amountYes, setAmountYes] = useState("");
  const [startSwap, setStartSwap] = useState(false);
  const [startSell, setStartSell] = useState(false);
  const [startReset, setStartReset] = useState(false);

  function resetVariables() {
    setStartSell(false);
    setStartSwap(false);
    setStartReset(false);
    window.location.reload();
  }

  useEffect(() => {
    const temp = addDecimalSixPlacesFromRightAndRemoveTrail(
      data["amountYes"].toString()
    );
    setAmountYes(temp);
  }, []);

  useEffect(() => {
    if (startReset) {
      resetVariables();
    }
  }, [startReset]);

  return (
    <>
      <Flex direction="column" align="center" gap={1}>
        <Text>Favour</Text>
        <Button
          bgColor="green.300"
          _hover={{ bgColor: "green.400", cursor: "default" }}
        >
          {amountYes}
        </Button>
        {amountYes.toString() != 0 ? (
          <>
            <Flex direction="row" gap={1}>
              <Button
                fontSize="12px"
                onClick={() => setStartSwap(true)}
                colorScheme="blue"
              >
                Swap To Against
              </Button>
              <Button
                fontSize="12px"
                onClick={() => setStartSell(true)}
                colorScheme="yellow"
              >
                Sell
              </Button>
            </Flex>
          </>
        ) : null}
      </Flex>
      {startSell ? (
        <Sell
          setStartReset={setStartReset}
          handler={data["marketHandler"]}
          tokenType="yes"
          amount={data["amountYes"]}
        />
      ) : null}
      {startSwap ? (
        <Swap
          setStartReset={setStartReset}
          handler={data["marketHandler"]}
          finalType="no"
          amount={data["amountYes"]}
        />
      ) : null}
    </>
  );
}
