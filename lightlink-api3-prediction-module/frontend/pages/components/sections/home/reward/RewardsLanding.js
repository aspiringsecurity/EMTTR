import { Box, Heading, Flex } from "@chakra-ui/react";
import RewardCard from "./RewardCard";

export default function RewardsLanding({ userData, concluded }) {
  return (
    <>
      <Box m={10} maxH="max-content" borderRadius={10}>
        <Flex direction="row" ml={5}>
          <Heading fontFamily="vt323" size="4xl" mb={5}>
            Your Winnings
          </Heading>
        </Flex>
        <Flex
          direction="row"
          w="100%"
          maxH="max-content"
          gap={5}
          borderRadius={10}
        >
          {concluded.map((item, index) => (
            <RewardCard key={index} data={userData[item]} />
          ))}
        </Flex>
      </Box>
    </>
  );
}
