import {ethers} from "ethers"
import {useEffect, useState} from "react"

type Web3 = {
    provider?: ethers.providers.Web3Provider,
    isLoading: boolean
    error: string
}


export const useWeb3 = (): Web3 => {
    const [provider, setProvider] = useState<ethers.providers.Web3Provider>()
    const [error, setError] = useState<string>("")
    const [isLoading, setLoading] = useState<boolean>(true)

    useEffect(() => {
        if (window.ethereum == null) {
            setError("You must have metamask installed!")
            return
        }

        const provider = new ethers.providers.Web3Provider(window.ethereum)
        provider.send("eth_requestAccounts", [])
            .then((accounts) => {
                console.log(`accounts loaded: ${accounts}`)
                setProvider(provider)
                setError("")
                setLoading(false)
            })
            .catch(error => {
                setError(`there was an error getting your metamask provider: ${error}`)
                setLoading(false)
            })

    }, [window.ethereum])

    return {
        provider,
        isLoading,
        error
    }
}