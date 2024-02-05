import { ConnectButton } from "@rainbow-me/rainbowkit";
import { useAccount } from "wagmi";

import { Flex, Spacer, Box, Text } from "@chakra-ui/react";

export default function HomeHeader({ setPage }) {
  const { isConnected } = useAccount();

  return (
    <>
      <Flex direction="row" margin={5} align="center" justify="center">
        <Box fontFamily="Barlow" fontSize={50}>
          PM
        </Box>
        <Spacer />
        {isConnected ? (
          <>
            <Flex direction="row" gap={10} mr={10}>
              <Text
                onClick={() => setPage("Home")}
                _hover={{ cursor: "pointer" }}
              >
                Home
              </Text>
              <Text
                onClick={() => setPage("Create")}
                _hover={{ cursor: "pointer" }}
              >
                Create
              </Text>
              <Text
                onClick={() => setPage("Buy")}
                _hover={{ cursor: "pointer" }}
              >
                Buy
              </Text>
              <Text
                onClick={() => setPage("Dashboard")}
                _hover={{ cursor: "pointer" }}
              >
                Dashboard
              </Text>
              <Text
                onClick={() => setPage("Rewards")}
                _hover={{ cursor: "pointer" }}
              >
                Rewards
              </Text>{" "}
              <Text
                onClick={() => setPage("Conclude")}
                _hover={{ cursor: "pointer" }}
              >
                Conclude
              </Text>
            </Flex>
          </>
        ) : null}
        <ConnectButton label="Connect" showBalance={false} />
      </Flex>
    </>
  );
}
