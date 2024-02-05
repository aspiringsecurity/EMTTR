import {
  Modal,
  ModalOverlay,
  ModalContent,
  ModalHeader,
  ModalFooter,
  ModalBody,
  ModalCloseButton,
  Button,
  FormLabel,
  Flex,
  useDisclosure,
  Input,
  Text,
} from "@chakra-ui/react";

import { usdcABI, usdcAddress, mhABI } from "@/constants/info";

import { addDecimalTwoPlacesFromRight } from "@/helper/functions";

import {
  usePrepareContractWrite,
  useContractWrite,
  useWaitForTransaction,
} from "wagmi";

import { useEffect, useState } from "react";

import { toast } from "react-toastify";

export default function YesModal({ mhAddress, price }) {
  const { isOpen, onOpen, onClose } = useDisclosure();

  const [bought, setBought] = useState(false);
  const [approved, setApproved] = useState(false);
  const [amount, setAmount] = useState(0n);
  const [estimate, setEstimate] = useState(undefined);

  function resetVariables() {
    setBought(false);
    setApproved(false);
    setAmount(0n);
    setEstimate(false);
  }

  function generateEstimate() {
    if (amount == 0n) {
      toast.info("Please Enter An Amount.");
    }
    setEstimate(addDecimalTwoPlacesFromRight((price * amount).toString()));
  }

  // BUY      ================

  const { data: buyYesTokenData, write: buyYesTokenWrite } = useContractWrite({
    address: mhAddress,
    abi: mhABI,
    functionName: "buyYesToken",
    args: [amount * 1000000n],
  });

  const buyYesTokenWait = useWaitForTransaction({
    hash: buyYesTokenData?.hash,
    onSuccess() {
      setBought(true);
    },
  });

  /// APPROVAL ===============

  const { config: approvalConfig } = usePrepareContractWrite({
    address: usdcAddress,
    abi: usdcABI,
    functionName: "approve",
    args: [mhAddress, price * amount * 10000n],
  });
  const { data: usdcApprovalData, write: usdcApprovalWrite } =
    useContractWrite(approvalConfig);

  const usdcApprovalWait = useWaitForTransaction({
    hash: usdcApprovalData?.hash,
    onSuccess() {
      console.log("Success Approval");
      setApproved(true);
    },
  });

  useEffect(() => {
    if (bought) {
      toast.success("Successfully Bought Yes Token(s).");
      onClose();
      resetVariables();
    }
  }, [bought]);

  useEffect(() => {
    if (approved) {
      buyYesTokenWrite();
    }
  }, [approved]);

  return (
    <>
      <Button bgColor="green.400" onClick={onOpen}>
        Yes
      </Button>
      <Modal
        isOpen={isOpen}
        onClose={() => {
          onClose();
          setEstimate(undefined);
        }}
      >
        <ModalOverlay />
        <ModalContent>
          <ModalHeader>Get An Estimate</ModalHeader>
          <ModalCloseButton />
          <ModalBody>
            <FormLabel mt={5} ml={1}>
              Amount Of Tokens
            </FormLabel>
            <Flex direction="row" gap={5}>
              <Input
                type="number"
                placeholder="10"
                onChange={(e) => setAmount(BigInt(e.target.value))}
              />
              <Button onClick={() => generateEstimate()} fontSize={15}>
                Calculate
              </Button>
            </Flex>
            {estimate ? (
              <Text color="gray" fontSize={14} mt={2} ml={1}>
                $ {estimate}
              </Text>
            ) : null}
          </ModalBody>
          <ModalFooter>
            <Button
              colorScheme="green"
              mr={3}
              onClick={() => {
                generateEstimate();
                if (amount != 0n) {
                  toast.info(
                    "Please click on the 'Use Default' when setting the approval."
                  );
                  usdcApprovalWrite();
                }
              }}
            >
              Initiate
            </Button>
          </ModalFooter>
        </ModalContent>
      </Modal>
    </>
  );
}
