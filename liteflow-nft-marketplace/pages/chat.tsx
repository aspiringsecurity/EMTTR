import { Box } from '@chakra-ui/react'
import { Account, Chat, ChatProvider } from '@nft/chat'
import request, { gql } from 'graphql-request'
import { NextPage } from 'next'
import { useCallback, useMemo } from 'react'
import Head from '../components/Head'
import useEnvironment from '../hooks/useEnvironment'
import useLoginRedirect from '../hooks/useLoginRedirect'
import useSigner from '../hooks/useSigner'
import LargeLayout from '../layouts/large'
import { getTheme } from '../styles/theme'

const accounts = new Map<string, Promise<Account>>()

const ChatPage: NextPage = () => {
  const { LITEFLOW_API_KEY, BRAND_COLOR } = useEnvironment()
  const signer = useSigner()
  useLoginRedirect()

  const lookupAddress = useCallback(
    async (address: string) => {
      const res = accounts.get(address)
      if (res) return res
      const promise = request<{
        account: {
          name?: string
          image?: string
        }
      }>(
        `${
          process.env.NEXT_PUBLIC_LITEFLOW_BASE_URL ||
          'https://api.liteflow.com'
        }/${LITEFLOW_API_KEY}/graphql`,
        gql`
          query LookupAccount($address: Address!) {
            account(address: $address) {
              name
              image
            }
          }
        `,
        { address: address.toLowerCase() },
      ).then(({ account }) => ({
        name: account?.name || undefined,
        avatar: account?.image || undefined,
      }))

      accounts.set(address, promise)
      return promise
    },
    [LITEFLOW_API_KEY],
  )

  const theme = useMemo(() => getTheme(BRAND_COLOR), [BRAND_COLOR])

  return (
    <LargeLayout>
      <Head title="Chat" />
      <Box
        borderBlock="1px"
        borderInline={{ base: 'none', lg: '1px' }}
        // Need color definition for both breakpoints for some reason.
        // borderColor="gray.200" doesn't apply for both.
        borderColor={{ base: 'gray.200', lg: 'gray.200' }}
        rounded={{ base: 'none', lg: 'xl' }}
        height="50vh"
        overflow="hidden"
        mx={{ base: -6, lg: 0 }}
      >
        <ChatProvider
          signer={signer}
          theme={theme}
          lookupAddress={lookupAddress}
        >
          <Chat />
        </ChatProvider>
      </Box>
    </LargeLayout>
  )
}

export default ChatPage
