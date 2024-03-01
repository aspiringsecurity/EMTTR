import { Chain } from 'viem'
import { Skeleton, Label } from '@eth-optimism/ui-components'

import type { Token } from '@eth-optimism/op-app'
import { useAccount } from 'wagmi'

import { useReadBalance } from '@/hooks/useReadBalance'

export type FromProps = {
  chain: Chain
  amount?: string
  selectedToken: Token
}

export const To = ({ chain, amount, selectedToken }: FromProps) => {
  const { address } = useAccount()

  const balance = useReadBalance({
    chain,
    selectedToken,
  })

  return (
    <div>
      <div className="from-label">
        <Label className="text-base">To:</Label> {chain.name}
        {amount && selectedToken && (
          <div className="text-sm">
            You will receive: {amount} {selectedToken.symbol.toUpperCase()}
          </div>
        )}
        <div className="flex flex-row items-center py-3">
          Balance:{' '}
          {!address || balance.isPending ? (
            <Skeleton className="h-4 w-[200px] mt-1 ml-1" />
          ) : (
            balance.data.formatted
          )}
        </div>
      </div>
    </div>
  )
}
