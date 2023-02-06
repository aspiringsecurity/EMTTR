import { BigNumber, Signer } from "ethers"
import { createContext, useContext, useEffect, useMemo, useState } from "react"

type Wallet = {
    address: null | string
    signer: null | Signer
    balance: null | BigNumber
}

const defaultWallet: Wallet = {
    address: null,
    signer: null,
    balance: null,
}

export const WalletContext = createContext({})

export const useWalletContext = () => {
    const [wallet, setWallet] = useState(defaultWallet)
    const [connected, setConnected] = useState(false)
    const disconnectWallet = () => setWallet(defaultWallet)

    useEffect(() => {
        if (wallet.signer) {
            ;(async () => {
                const address = await wallet.signer.getAddress()
                const balance = await wallet.signer.getBalance()              
                setWallet({ ...wallet, address, balance })
            })()
        }
        setConnected(!!wallet.signer)
    }, [wallet.signer])

    const setWalletSigner = (signer: Signer) => setWallet({ ...wallet, signer })

    const walletContext = useMemo(
        () => ({
            wallet,
            connected,
            setWallet,
            setWalletSigner,
            disconnectWallet,
        }),
        [wallet, connected, setWallet, setWalletSigner, disconnectWallet]
    )

    return walletContext
}

const useWallet = () => {
    return useContext(WalletContext)
}

export default useWallet
