import { Flex, Icon, Text } from '@chakra-ui/react'
import { HiBadgeCheck } from '@react-icons/all-files/hi/HiBadgeCheck'
import { FC } from 'react'
import { AccountVerificationStatus } from '../../graphql'
import Link from '../Link/Link'
import WalletAddress from '../Wallet/Address'
import AccountImage from '../Wallet/Image'

type Props = {
  user: {
    address: string
    name: string | null
    image: string | null
    verification: {
      status: AccountVerificationStatus
    } | null
  }
  size?: number
}

const Avatar: FC<Props> = ({
  user: { address, name, image, verification },
  size = 8,
}) => {
  return (
    <Link display="block" flexShrink={0} href={`/users/${address}`}>
      <Flex align="center" gap={2}>
        <Flex
          as={AccountImage}
          address={address}
          image={image}
          size={size * 4}
          rounded="full"
        />
        <Text as="span" variant="subtitle2" color="gray.500">
          {name || <WalletAddress address={address} isShort />}
        </Text>
        {verification?.status === 'VALIDATED' && (
          <Flex align="center" h={4} w={4}>
            <Icon as={HiBadgeCheck} color="brand.500" />
          </Flex>
        )}
      </Flex>
    </Link>
  )
}

export default Avatar
