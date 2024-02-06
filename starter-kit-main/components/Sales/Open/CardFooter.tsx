import { Box, Text } from '@chakra-ui/react'
import useTranslation from 'next-translate/useTranslation'
import { FC, HTMLAttributes } from 'react'
import Link from '../../Link/Link'
import Price from '../../Price/Price'

type Props = {
  assetId: string
  bestBid:
    | {
        unitPrice: string
        currency: {
          decimals: number
          symbol: string
        }
      }
    | undefined
  isOwner: boolean
  showButton?: boolean
}

const SaleOpenCardFooter: FC<HTMLAttributes<any> & Props> = ({
  assetId,
  bestBid,
  isOwner,
  showButton = true,
  ...props
}) => {
  const { t } = useTranslation('components')
  return (
    <Box
      as={Link}
      href={`/tokens/${assetId}${!isOwner ? '/bid' : ''}`}
      py={2}
      px={4}
      bgColor={showButton ? 'brand.500' : 'gray.100'}
      {...props}
    >
      <Text
        variant="subtitle2"
        color={showButton ? 'white' : 'gray.500'}
        noOfLines={1}
        wordBreak="break-all"
      >
        {showButton ? (
          isOwner ? (
            t('sales.open.card-footer.view')
          ) : (
            t('sales.open.card-footer.place-bid')
          )
        ) : bestBid ? (
          <>
            <Text as="span" variant="subtitle2" mr={1}>
              {t('sales.open.card-footer.highest-bid')}
            </Text>
            <Text
              as={Price}
              variant="subtitle2"
              amount={bestBid.unitPrice}
              currency={bestBid.currency}
              color="brand.black"
            />
          </>
        ) : (
          t('sales.open.card-footer.open')
        )}
      </Text>
    </Box>
  )
}

export default SaleOpenCardFooter
