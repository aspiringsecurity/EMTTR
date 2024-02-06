import { Flex, Icon, Text, useDisclosure, useToast } from '@chakra-ui/react'
import { BigNumber } from '@ethersproject/bignumber'
import { CancelOfferStep, useCancelOffer } from '@liteflow/react'
import { HiBadgeCheck } from '@react-icons/all-files/hi/HiBadgeCheck'
import useTranslation from 'next-translate/useTranslation'
import { FC, useCallback } from 'react'
import { AccountVerificationStatus } from '../../../graphql'
import useAccount from '../../../hooks/useAccount'
import useBlockExplorer from '../../../hooks/useBlockExplorer'
import useSigner from '../../../hooks/useSigner'
import { formatDate, formatError, isSameAddress } from '../../../utils'
import CheckoutButton from '../../Button/CheckoutButton'
import ConnectButtonWithNetworkSwitch from '../../Button/ConnectWithNetworkSwitch'
import Link from '../../Link/Link'
import { ListItem } from '../../List/List'
import CancelOfferModal from '../../Modal/CancelOffer'
import Price from '../../Price/Price'
import WalletAddress from '../../Wallet/Address'
import AccountImage from '../../Wallet/Image'

export type Props = {
  chainId: number
  sale: {
    id: string
    expiredAt: Date
    maker: {
      address: string
      image: string | null
      name: string | null
      verification: {
        status: AccountVerificationStatus
      } | null
    }
    unitPrice: string
    availableQuantity: string
    currency: {
      decimals: number
      symbol: string
    }
  }
  onOfferCanceled: (id: string) => Promise<void>
}

const SaleDirectModalItem: FC<Props> = ({ sale, chainId, onOfferCanceled }) => {
  const { t } = useTranslation('components')
  const signer = useSigner()
  const { address } = useAccount()
  const blockExplorer = useBlockExplorer(chainId)
  const [cancelOffer, { activeStep, transactionHash }] = useCancelOffer(signer)
  const toast = useToast()
  const { isOpen, onOpen, onClose } = useDisclosure()

  const handleCancel = useCallback(async () => {
    if (!confirm(t('sales.direct.modal-item.cancel-confirmation'))) return
    try {
      onOpen()
      await cancelOffer(sale.id)
      await onOfferCanceled(sale.id)
    } catch (e) {
      toast({
        title: formatError(e),
        status: 'error',
      })
    } finally {
      onClose()
    }
  }, [cancelOffer, onClose, onOfferCanceled, onOpen, sale, t, toast])

  return (
    <>
      <ListItem
        image={
          <Flex as={Link} href={`/users/${sale.maker.address}`}>
            <Flex
              as={AccountImage}
              address={sale.maker.address}
              image={sale.maker.image}
              size={40}
              rounded="full"
            />
          </Flex>
        }
        label={
          <Flex
            display="inline-flex"
            align="center"
            gap={1.5}
            as={Link}
            href={`/users/${sale.maker.address}`}
          >
            <Text
              as="span"
              color="brand.black"
              fontWeight="medium"
              title={sale.maker.name || sale.maker.address}
            >
              {sale.maker.name || (
                <WalletAddress address={sale.maker.address} isShort />
              )}
            </Text>
            {sale.maker.verification?.status === 'VALIDATED' && (
              <Icon as={HiBadgeCheck} color="brand.500" h={4} w={4} />
            )}
          </Flex>
        }
        subtitle={
          <Flex direction="column" as="span">
            <span>
              <Text
                as={Price}
                color="brand.black"
                fontWeight="medium"
                amount={sale.unitPrice}
                currency={sale.currency}
              />
              {t('sales.direct.modal-item.per-edition')}
            </span>
            <span>
              {t('sales.direct.modal-item.available', {
                count: BigNumber.from(sale.availableQuantity).lte(
                  Number.MAX_SAFE_INTEGER - 1,
                )
                  ? BigNumber.from(sale.availableQuantity).toNumber()
                  : Number.MAX_SAFE_INTEGER - 1,
              })}
            </span>
          </Flex>
        }
        caption={
          sale.expiredAt ? (
            <Text as="span" color="gray.400">
              {t('sales.direct.modal-item.expiration', {
                date: formatDate(sale.expiredAt),
              })}
            </Text>
          ) : undefined
        }
        action={
          !!address && isSameAddress(sale.maker.address, address) ? (
            <ConnectButtonWithNetworkSwitch
              chainId={chainId}
              variant="outline"
              colorScheme="gray"
              w={{ base: 'full', md: 'auto' }}
              onClick={() => handleCancel()}
              isLoading={activeStep !== CancelOfferStep.INITIAL}
            >
              <Text as="span" isTruncated>
                {t('sales.direct.modal-item.cancel')}
              </Text>
            </ConnectButtonWithNetworkSwitch>
          ) : (
            <CheckoutButton offer={sale}>
              <Text as="span" isTruncated>
                {t('sales.direct.modal-item.buy')}
              </Text>
            </CheckoutButton>
          )
        }
      />

      <CancelOfferModal
        isOpen={isOpen}
        onClose={onClose}
        title={t('sales.direct.modal-item.cancel')}
        step={activeStep}
        blockExplorer={blockExplorer}
        transactionHash={transactionHash}
      />
    </>
  )
}

export default SaleDirectModalItem
