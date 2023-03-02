import { ConnectKitButton } from 'connectkit'
import { useRef, useState } from 'react'
import Jazzicon from 'react-jazzicon/dist/Jazzicon'
import { Link, useLocation } from 'react-router-dom'
import { useAccount, useBalance, useDisconnect } from 'wagmi'
import { Button, Sprite } from '../../components'
import { useOutsideAlerter } from '../../hooks'
import { NAV_LINKS } from '../../utils/constants'
import './header.scss'

const Header = () => {
  const { address: accountAddress, isConnected } = useAccount()
  const { data: userBalance } = useBalance({
    address: accountAddress,
    formatUnits: 'ether',
  })
  const { disconnect } = useDisconnect()

  const [showDropdown, setShowDropdown] = useState(false)
  const dropdownRef = useRef(null)
  const location = useLocation()

  useOutsideAlerter(dropdownRef, () => setShowDropdown(false))

  return (
    <div className="header">
      <div className="logo-area">
        <img
          src="/assets/svgs/opdready-icon.svg"
          alt="opdready-icon"
          width={70}
          height={70}
        />
        <div className="links">
          {NAV_LINKS.map((link) => (
            <Link key={link.label} to={`${link.link}`} className={`link ${link.link === location.pathname ? 'active' : ''}`}>
              {link.label}
            </Link>
          ))}
        </div>
      </div>
      <div className="connect-button-dropdown" ref={dropdownRef}>
        {isConnected ? (
          <div className="wallet-dropdown-button">
            <button
              className="wallet-details"
              onClick={() => setShowDropdown(!showDropdown)}
            >
              <Jazzicon diameter={24} seed={40} />
              <label className="label">
                {accountAddress?.slice(0, 4)}...{accountAddress?.slice(-4)}
              </label>
              <Sprite
                id="dropdown-expand-icon"
                width={24}
                height={24}
                {...(showDropdown
                  ? { style: { transform: 'rotate(180deg)' } }
                  : undefined)}
              />
            </button>
          </div>
        ) : (
          <ConnectKitButton.Custom>
            {({ show }) => {
              return <Button onClick={show}>Connect</Button>
            }}
          </ConnectKitButton.Custom>
        )}
        {showDropdown ? (
          <div className="wallet-dropdown">
            <div className="wallet-info">
              <Jazzicon diameter={74} seed={40} />
              <div className="wallet-name-balance">
                <label className="wallet-name">
                  {`${accountAddress?.slice(0, 4)}...${accountAddress?.slice(
                    -4,
                  )}`}
                </label>
                <label className="wallet-balance">
                  {userBalance?.formatted.slice(0, 8)}
                </label>
              </div>
              <div className="wallet-actions">
                <div
                  className="action"
                  onClick={() => {
                    window.navigator.clipboard.writeText(accountAddress || '')
                    setShowDropdown(false)
                  }}
                >
                  <Sprite id="copy-address-icon" width={19} height={18} />
                  <label className="action-label">Copy</label>
                </div>
                <div
                  className="action"
                  onClick={() => {
                    disconnect()
                    setShowDropdown(false)
                  }}
                >
                  <Sprite id="disconnect-wallet-icon" width={19} height={18} />
                  <label className="action-label">Disconnect</label>
                </div>
              </div>
            </div>
          </div>
        ) : null}
      </div>
    </div>
  )
}

export default Header
