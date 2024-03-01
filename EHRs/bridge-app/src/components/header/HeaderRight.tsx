import { AccountMenu } from '@/components/AccountMenu'
import { ThemeToggle } from '@/components/ThemeToggle'
import { Separator } from '@eth-optimism/ui-components'

export const HeaderRight = () => {
  return (
    <div className="account-menu flex flex-row">
      <div className="space-x-3 hidden md:flex">
        <ThemeToggle />
        <Separator orientation="vertical" />
      </div>
      <div className="flex flex-row items-center pl-6">
        <AccountMenu />
      </div>
    </div>
  )
}
