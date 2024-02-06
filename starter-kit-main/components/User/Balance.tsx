import { Flex, Heading, Icon, Spinner, Text, VStack } from '@chakra-ui/react'
import { IoWalletOutline } from '@react-icons/all-files/io5/IoWalletOutline'
import useTranslation from 'next-translate/useTranslation'
import { FC } from 'react'
import useBalance from '../../hooks/useBalance'
import Price from '../Price/Price'

type Props = {
  account: string
  currency: {
    id: string
    decimals: number
    symbol: string
  }
}

const Balance: FC<Props> = ({ account, currency }) => {
  const { t } = useTranslation('components')
  const [balance] = useBalance(account, currency?.id)
  return (
    <VStack align="flex-start" spacing={4}>
      <Flex
        display="inline-flex"
        wrap="wrap"
        align="center"
        rounded="full"
        py={2}
        px={4}
        bgColor="brand.50"
      >
        <Icon as={IoWalletOutline} color="brand.black" mr={3} h={4} w={4} />
        <Heading as="span" variant="heading3" color="gray.500" mr={2}>
          {t('user.balance.title')}
        </Heading>
        {balance === undefined ? (
          <Spinner
            color="brand.black"
            mr={2}
            size="sm"
            thickness="2px"
            speed="0.65s"
          />
        ) : (
          <Heading as="h5" variant="heading3" color="brand.black">
            <Text
              as={Price}
              fontWeight="semibold"
              amount={balance}
              currency={currency}
            />
          </Heading>
        )}
      </Flex>
    </VStack>
  )
}

export default Balance
