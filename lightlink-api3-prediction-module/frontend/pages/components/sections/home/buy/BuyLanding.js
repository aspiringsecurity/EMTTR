import { Box } from "@chakra-ui/react";
import BuyCard from "./BuyCard";

export default function BuyLanding({ ids, length }) {
  return (
    <>
      <Box m={10} maxH="max-content" bgColor="#F0FFF0" borderRadius={10}>
        {ids
          ? ids.map((item, index) => <BuyCard key={index} data={item} />)
          : null}
      </Box>
    </>
  );
}
