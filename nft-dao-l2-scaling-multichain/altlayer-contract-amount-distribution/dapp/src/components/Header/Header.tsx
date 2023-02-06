import React, { useEffect } from "react"
import { ethers, Signer } from "ethers"
import { truncateEthAddress } from "utils/helpers"
import Button from "components/shared/Button"
import useWallet from "utils/context/walletContext"
import useHackatonManagerFactory from "utils/context/hackatonManagerFactoryContext"
import useHackatonManager from "utils/context/hackatonManagerContext"
import Link from "next/link"

const Header: React.FC = () => {
    const shouldDisConnect = false
    const { wallet, setWalletSigner, disconnectWallet } = useWallet()
    const {
        createSignedContract: createSignedFactoryContract,
        resetSignedContract: resetSignedFactoryContract,
    } = useHackatonManagerFactory()
    const { createSignedContract, resetSignedContract } = useHackatonManager()

    useEffect(() => {
        if (window) {
            window.ethereum.on("accountsChanged", function (accounts) {
                handleConnectWallet()
            })
            window.ethereum.on("chainChanged", function (accounts) {
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

    return (
        <div className="container mx-auto py-2 flex justify-between items-center">
            <Link href="/list-hackaton">
                <Button onClick={() => null}>Hackatons overview</Button>
            </Link>

            <Link href="/create-hackaton">
                <Button onClick={() => null}>Create new hackaton</Button>
            </Link>
            {/* <div>
                <Button onClick={wallet.address ? handleDisconnectWallet : handleConnectWallet}>
                    {wallet.address ? truncateEthAddress(wallet.address) : "Connect"}
                </Button>
                {wallet.address && wallet.balance ? (
                    <span className="bg-blue-200 text-blue-800 -m-4 py-2 px-4 pl-6 font-bold rounded-r-xl inline-flex items-center">
                        {(+ethers.utils.formatEther(wallet.balance)).toFixed(2)} ETH
                    </span>
                ) : null}
            </div> */}
        </div>
    )
}
export default Header
