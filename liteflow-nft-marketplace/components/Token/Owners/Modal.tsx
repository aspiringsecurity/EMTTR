import {
  Box,
  Flex,
  Modal,
  ModalBody,
  ModalCloseButton,
  ModalContent,
  ModalFooter,
  ModalHeader,
  ModalOverlay,
  SkeletonCircle,
  SkeletonText,
  Text,
  useDisclosure,
} from '@chakra-ui/react'
import useTranslation from 'next-translate/useTranslation'
import { FC, useCallback, useState } from 'react'
import {
  AccountVerificationStatus,
  useFetchOwnersLazyQuery,
} from '../../../graphql'
import List, { ListItem } from '../../List/List'
import Pagination from '../../Pagination/Pagination'
import OwnersModalActivator from './ModalActivator'
import OwnersModalItem from './ModalItem'

export type Props = {
  asset: {
    chainId: number
    collectionAddress: string
    tokenId: string
    ownerships: {
      totalCount: number
      nodes: {
        ownerAddress: string
        quantity: string
        owner: {
          address: string
          name: string | null
          image: string | null
          verification: {
            status: AccountVerificationStatus
          } | null
        }
      }[]
    }
  }
}

const OwnerPaginationLimit = 8

const OwnersModal: FC<Props> = ({ asset }) => {
  const { t } = useTranslation('components')
  const { isOpen, onOpen, onClose } = useDisclosure()
  const [page, setPage] = useState(1)

  const [fetch, { data }] = useFetchOwnersLazyQuery({
    variables: {
      chainId: asset.chainId,
      collectionAddress: asset.collectionAddress,
      tokenId: asset.tokenId,
      limit: OwnerPaginationLimit,
      offset: (page - 1) * OwnerPaginationLimit,
    },
  })

  const openOwners = useCallback(async () => {
    onOpen()
    await fetch()
  }, [fetch, onOpen])

  const closeOwners = useCallback(() => {
    onClose()
    setPage(1)
  }, [onClose])

  return (
    <>
      <OwnersModalActivator
        ownerships={asset.ownerships}
        onClick={openOwners}
      />
      <Modal
        isOpen={isOpen}
        onClose={closeOwners}
        isCentered
        size="xl"
        scrollBehavior="inside"
      >
        <ModalOverlay />
        <ModalContent>
          <ModalHeader>
            <Flex>
              {t('token.owners.title')}
              <Flex
                bgColor="brand.50"
                my="auto"
                ml={3}
                align="center"
                justify="center"
                rounded="lg"
                py={0.5}
                px={2.5}
              >
                <Text as="span" variant="caption" color="brand.500">
                  {asset.ownerships.totalCount}
                </Text>
              </Flex>
            </Flex>
          </ModalHeader>
          <ModalCloseButton />
          <ModalBody
            maxHeight={{ base: '', md: 'lg' }}
            minHeight={{ base: '', md: 'lg' }}
          >
            <List>
              {!data
                ? new Array(OwnerPaginationLimit)
                    .fill(0)
                    .map((_, index) => (
                      <ListItem
                        key={index}
                        image={<SkeletonCircle />}
                        label={<SkeletonText noOfLines={2} width="32" />}
                      />
                    ))
                : data.ownerships?.nodes.map((ownership) => (
                    <OwnersModalItem
                      key={ownership.ownerAddress}
                      ownership={ownership}
                    />
                  ))}
            </List>
          </ModalBody>
          <ModalFooter>
            <Box pt="4">
              <Pagination
                page={page}
                onPageChange={setPage}
                hasNextPage={data?.ownerships?.pageInfo.hasNextPage}
                hasPreviousPage={data?.ownerships?.pageInfo.hasPreviousPage}
                withoutLimit
              />
            </Box>
          </ModalFooter>
        </ModalContent>
      </Modal>
    </>
  )
}

export default OwnersModal
