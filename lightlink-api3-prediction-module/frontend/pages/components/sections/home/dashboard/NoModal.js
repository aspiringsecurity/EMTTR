import { addDecimalSixPlacesFromRightAndRemoveTrail } from "@/helper/functions";
import { Button, Text, Flex } from "@chakra-ui/react";

import { useEffect, useState } from "react";

import Sell from "./Sell";
import Swap from "./Swap";

export default function NoModal({ data }) {
  const [amountNo, setAmountNo] = useState("");
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
      data["amountNo"].toString()
    );
    setAmountNo(temp);
  }, []);

  useEffect(() => {
    if (startReset) {
      resetVariables();
    }
  }, [startReset]);

  return (
    <>
      <Flex direction="column" align="center" gap={1}>
        <Text>Against</Text>
        <Button
          bgColor="red.300"
          _hover={{ bgColor: "red.400", cursor: "default" }}
        >
          {amountNo}
        </Button>
        {amountNo.toString() != 0 ? (
          <>
            <Flex direction="row" gap={1}>
              <Button
                fontSize="12px"
                onClick={() => setStartSwap(true)}
                colorScheme="blue"
              >
                Swap To Favour
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
          tokenType="no"
          amount={data["amountNo"]}
        />
      ) : null}
      {startSwap ? (
        <Swap
          setStartReset={setStartReset}
          handler={data["marketHandler"]}
          finalType="yes"
          amount={data["amountNo"]}
        />
      ) : null}
    </>
  );
}
