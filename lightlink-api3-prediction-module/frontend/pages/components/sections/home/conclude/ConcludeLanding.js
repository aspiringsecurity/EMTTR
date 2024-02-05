import { Flex, Heading, Box } from "@chakra-ui/react";
import ConcludeCard from "./ConcludeCard";
import { useEffect, useState } from "react";

import { FadeInWhenVisible } from "@/pages/components/TransitionBoxes";

export default function ConcludeLanding({ ids, concludedArray }) {
  console.log(ids, concludedArray);
  const [finalArray, setFinalArray] = useState([]);

  useEffect(() => {
    async function execute() {
      const finalTemp = [];

      if (concludedArray.length == 0) setFinalArray(Object.keys(ids));
      else {
        const keys = Object.keys(ids);
        const limit = keys.length;
        for (var index = 0; index < limit; index++) {
          const element = keys[index];
          if (!concludedArray.includes(element)) {
            finalTemp.push(element);
          }
        }

        if (finalTemp.length != 0) setFinalArray(finalTemp);
      }
    }

    execute();
  }, []);

  return (
    <>
      {finalArray.length != 0 ? (
        <>
          <Box m={10} maxH="max-content" bgColor="#F0FFF0" borderRadius={10}>
            {finalArray.map((item, index) => (
              <ConcludeCard key={index} id={item} />
            ))}
          </Box>
        </>
      ) : (
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
                It looks like there are no markets to be concluded :)
              </Heading>
            </Flex>
          </FadeInWhenVisible>
        </>
      )}
    </>
  );
}
