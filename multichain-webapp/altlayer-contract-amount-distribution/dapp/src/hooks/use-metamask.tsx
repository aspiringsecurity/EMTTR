import { ethers, Signer } from "ethers"
import { truncateEthAddress } from "utils/helpers"
import Button from "components/shared/Button"
import useWallet from "utils/context/walletContext"
import useHackatonManagerFactory from "utils/context/hackatonManagerFactoryContext"
import useHackatonManager from "utils/context/hackatonManagerContext"
import Link from "next/link"
import { useEffect } from "react"

const useMetamask = () => {
    const shouldDisConnect = true
    const { wallet, setWalletSigner, disconnectWallet }: any = useWallet()
    const {
        createSignedContract: createSignedFactoryContract,
        resetSignedContract: resetSignedFactoryContract,
    } = useHackatonManagerFactory()
    const { createSignedContract, resetSignedContract } = useHackatonManager()

    useEffect(() => {
        if (window) {
            window.ethereum.on("accountsChanged", function () {
                handleConnectWallet()
            })
            window.ethereum.on("chainChanged", function () {
                handleConnectWallet()
            })
        }
    }, [])

    useEffect(() => {
        if (wallet && wallet.signer) {
            createSignedFactoryContract(wallet.signer)
            createSignedContract(wallet.signer)
        } else {
            resetSignedContract()
            resetSignedFactoryContract()
        }
    }, [wallet])

    const handleConnectWallet = () => {
        if (window.ethereum) {
            const provider = new ethers.providers.Web3Provider(window.ethereum)

            provider.send("eth_requestAccounts", []).then(async () => {
                await handleChangeAccount(provider.getSigner())
            })
        }
    }

    const handleDisconnectWallet = () => {
        shouldDisConnect && disconnectWallet()
    }

    const handleChangeAccount = async (newAccount: Signer) => {
        setWalletSigner(newAccount)
    }
    return { wallet, handleConnectWallet, handleDisconnectWallet }
}

export default useMetamask
