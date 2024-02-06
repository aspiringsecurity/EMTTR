import {
  Divider,
  Flex,
  Icon,
  Text,
  useDisclosure,
  useToast,
} from '@chakra-ui/react'
import { BigNumber, BigNumberish } from '@ethersproject/bignumber'
import {
  AcceptOfferStep,
  CancelOfferStep,
  useAcceptOffer,
  useCancelOffer,
} from '@liteflow/react'
import { HiBadgeCheck } from '@react-icons/all-files/hi/HiBadgeCheck'
import Trans from 'next-translate/Trans'
import useTranslation from 'next-translate/useTranslation'
import { FC, SyntheticEvent, useMemo } from 'react'
import { AccountVerificationStatus, Standard } from '../../graphql'
import useAccount from '../../hooks/useAccount'
import useBlockExplorer from '../../hooks/useBlockExplorer'
import useSigner from '../../hooks/useSigner'
import {
  dateFromNow,
  formatDate,
  formatError,
  isSameAddress,
} from '../../utils'
import ConnectButtonWithNetworkSwitch from '../Button/ConnectWithNetworkSwitch'
import Link from '../Link/Link'
import { ListItem } from '../List/List'
import AcceptOfferModal from '../Modal/AcceptOffer'
import CancelOfferModal from '../Modal/CancelOffer'
import Price from '../Price/Price'
import WalletAddress from '../Wallet/Address'
import AccountImage from '../Wallet/Image'
import BidAcceptModal from './AcceptModal'

export type Props = {
  asset: {
    collection: {
      chainId: number
      standard: Standard
    }
    owned: {
      quantity: string
    } | null
  }
  bid: {
    id: string
    createdAt: Date
    expiredAt: Date | null
    unitPrice: string
    availableQuantity: string
    currency: {
      decimals: number
      symbol: string
    }
    maker: {
      address: string
      name: string | null
      image: string | null
      verification: {
        status: AccountVerificationStatus
      } | null
    }
  }
  preventAcceptation: boolean
  onAccepted: (id: string) => Promise<void>
  onCanceled: (id: string) => Promise<void>
}

const Bid: FC<Props> = ({
  asset,
  bid,
  preventAcceptation,
  onAccepted,
  onCanceled,
}) => {
  const { t } = useTranslation('components')
  const signer = useSigner()
  const { address: account } = useAccount()
  const toast = useToast()
  const blockExplorer = useBlockExplorer(asset.collection.chainId)
  const {
    isOpen: acceptOfferIsOpen,
    onOpen: acceptOfferOnOpen,
    onClose: acceptOfferOnClose,
  } = useDisclosure()
  const {
    isOpen: cancelOfferIsOpen,
    onOpen: cancelOfferOnOpen,
    onClose: cancelOfferOnClose,
  } = useDisclosure()
  const {
    isOpen: confirmAcceptIsOpen,
    onOpen: confirmAcceptOnOpen,
    onClose: confirmAcceptOnClose,
  } = useDisclosure()

  const [
    acceptOffer,
    { activeStep: activeAcceptOfferStep, transactionHash: acceptOfferHash },
  ] = useAcceptOffer(signer)
  const [
    cancelOffer,
    { activeStep: activeCancelOfferStep, transactionHash: cancelOfferHash },
  ] = useCancelOffer(signer)

  const isSingle = asset.collection.standard === 'ERC721'

  const canAccept = useMemo(() => {
    if (!account) return false
    if (preventAcceptation) return false
    return !isSameAddress(bid.maker.address, account)
  }, [account, bid, preventAcceptation])

  const canCancel = useMemo(() => {
    if (!account) return false
    return isSameAddress(account, bid.maker.address)
  }, [account, bid])

  const acceptBid = async (quantity?: BigNumberish) => {
    if (!canAccept) return
    if (activeAcceptOfferStep !== AcceptOfferStep.INITIAL) return
    try {
      acceptOfferOnOpen()
      confirmAcceptOnClose()
      await acceptOffer(bid.id, quantity || bid.availableQuantity)
      await onAccepted(bid.id)
    } catch (e) {
      toast({
        title: formatError(e),
        status: 'error',
      })
    } finally {
      acceptOfferOnClose()
    }
  }

  const cancelBid = async (e: SyntheticEvent) => {
    e.stopPropagation()
    e.preventDefault()
    if (!canCancel) return
    if (activeCancelOfferStep !== CancelOfferStep.INITIAL) return
    try {
      cancelOfferOnOpen()
      await cancelOffer(bid.id)
      await onCanceled(bid.id)
    } catch (e) {
      toast({
        title: formatError(e),
        status: 'error',
      })
    } finally {
      cancelOfferOnClose()
    }
  }

  return (
    <>
      <ListItem
        image={
          <Flex as={Link} href={`/users/${bid.maker.address}`}>
            <Flex
              as={AccountImage}
              address={bid.maker.address}
              image={bid.maker.image}
              size={40}
              rounded="full"
            />
          </Flex>
        }
        label={
          <Flex gap={2}>
            <span>
              <Trans
                ns="components"
                i18nKey={
                  isSingle
                    ? 'bid.detail.per-edition-single'
                    : 'bid.detail.per-edition-multiple'
                }
                components={[
                  <Text
                    as={Price}
                    amount={bid.unitPrice}
                    currency={bid.currency}
                    color="black"
                    fontWeight="medium"
                    key="price"
                  />,
                ]}
              />
            </span>
            {!isSingle && (
              <Flex as="span">
                <Divider orientation="vertical" />
                <span>
                  <Trans
                    ns="components"
                    i18nKey="bid.detail.requested"
                    components={[
                      <Text
                        as="span"
                        ml={2}
                        color="black"
                        fontWeight="medium"
                        key="quantity"
                      />,
                    ]}
                    values={{ quantity: bid.availableQuantity.toString() }}
                  />
                </span>
              </Flex>
            )}
          </Flex>
        }
        subtitle={
          <Link href={`/users/${bid.maker.address}`}>
            <Flex as="span" align="center" gap={1.5} cursor="pointer">
              <Text
                as="span"
                title={bid.maker.name || bid.maker.address}
                fontWeight="medium"
                color="black"
                fontSize="sm"
              >
                {bid.maker.name || (
                  <WalletAddress address={bid.maker.address} isShort />
                )}
              </Text>
              {bid.maker.verification?.status === 'VALIDATED' && (
                <Icon as={HiBadgeCheck} color="brand.500" h={4} w={4} />
              )}
            </Flex>
          </Link>
        }
        caption={
          <Text as="span" color="gray.400">
            {dateFromNow(bid.createdAt)}
            {bid.expiredAt &&
              t('bid.detail.expires', { date: formatDate(bid.expiredAt) })}
          </Text>
        }
        action={
          <>
            {canAccept && (
              <ConnectButtonWithNetworkSwitch
                chainId={asset.collection.chainId}
                w={{ base: 'full', md: 'auto' }}
                isLoading={activeAcceptOfferStep !== AcceptOfferStep.INITIAL}
                onClick={() =>
                  BigNumber.from(bid.availableQuantity).gt(1)
                    ? confirmAcceptOnOpen()
                    : acceptBid()
                }
              >
                <Text as="span" isTruncated>
                  {t('bid.detail.accept')}
                </Text>
              </ConnectButtonWithNetworkSwitch>
            )}
            {canCancel && (
              <ConnectButtonWithNetworkSwitch
                chainId={asset.collection.chainId}
                variant="outline"
                colorScheme="gray"
                w={{ base: 'full', md: 'auto' }}
                isLoading={activeCancelOfferStep !== CancelOfferStep.INITIAL}
                onClick={cancelBid}
              >
                <Text as="span" isTruncated>
                  {t('bid.detail.cancel')}
                </Text>
              </ConnectButtonWithNetworkSwitch>
            )}
          </>
        }
      />

      <AcceptOfferModal
        isOpen={acceptOfferIsOpen}
        onClose={acceptOfferOnClose}
        title={t('bid.detail.modal.accept.title')}
        step={activeAcceptOfferStep}
        blockExplorer={blockExplorer}
        transactionHash={acceptOfferHash}
      />
      <CancelOfferModal
        isOpen={cancelOfferIsOpen}
        onClose={cancelOfferOnClose}
        title={t('bid.detail.modal.cancel.title')}
        step={activeCancelOfferStep}
        blockExplorer={blockExplorer}
        transactionHash={cancelOfferHash}
      />
      <BidAcceptModal
        isOpen={confirmAcceptIsOpen}
        onClose={confirmAcceptOnClose}
        acceptBid={acceptBid}
        bid={bid}
        totalOwned={BigNumber.from(asset.owned?.quantity || 0)}
      />
    </>
  )
}

export default Bid
