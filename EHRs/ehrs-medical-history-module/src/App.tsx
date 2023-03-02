import { BrowserRouter, Route, Routes } from 'react-router-dom'
import { configureChains } from '@wagmi/core'
import './style/app.scss'
import Wrapper from './wrapper'
import { WagmiConfig, createClient } from 'wagmi'
import { ConnectKitProvider } from 'connectkit'
import { polygonMumbai } from '@wagmi/chains'
import { infuraProvider } from 'wagmi/providers/infura'
import { publicProvider } from 'wagmi/providers/public'
import { MetaMaskConnector } from 'wagmi/connectors/metaMask'
import { WalletConnectConnector } from 'wagmi/connectors/walletConnect'

function App() {
  const { chains, provider, webSocketProvider } = configureChains(
    [polygonMumbai],
    [
      infuraProvider({ apiKey: '01377e1e58094354aac3e16a311b71cc', priority: 0 }),
      publicProvider({ priority: 1 }),
    ],
  )

  const client = createClient({
    autoConnect: true,
    connectors: [
      new MetaMaskConnector({
        chains,
      }),
      new WalletConnectConnector({
        chains,
        options: {
          qrcode: true,
        },
      }),
    ],
    provider,
    webSocketProvider,
  })

  return (
    <WagmiConfig client={client}>
      <ConnectKitProvider theme="soft">
        <div className="app">
          <BrowserRouter>
            <Routes>
              <Route path="/*" element={<Wrapper />} />
            </Routes>
          </BrowserRouter>
        </div>
      </ConnectKitProvider>
    </WagmiConfig>
  )
}

export default App
