import { useEffect, useMemo } from "react"
import type { AppProps } from "next/app"
import { ToastContainer } from "react-toastify"
import "react-toastify/dist/ReactToastify.css"
import { useWalletContext, WalletContext } from "utils/context/walletContext"
import {
    useHackatonManagerFactoryContext,
    HackatonManagerFactoryContext,
} from "utils/context/hackatonManagerFactoryContext"
import {
    useHackatonManagerContext,
    HackatonManagerContext,
} from "utils/context/hackatonManagerContext"
import Layout from "components/Layout"
import "../styles/globals.css"
import { useRouter } from "next/router"

const MyApp = ({ Component, pageProps }: AppProps) => {
    const walletContext = useWalletContext()
    const hackatonManagerFactoryContext = useHackatonManagerFactoryContext()
    const hackatonManagerContext = useHackatonManagerContext()
    const router = useRouter()

    useEffect(() => {
        hackatonManagerFactoryContext.initHackatonManagerFactory()
    }, [])

    const Wrapper = useMemo(() => {
        if (router.pathname == "/") return (props: any) => <>{props.children}</>
        return Layout
    }, [router])

    return (
        <HackatonManagerFactoryContext.Provider value={hackatonManagerFactoryContext}>
            <HackatonManagerContext.Provider value={hackatonManagerContext}>
                <WalletContext.Provider value={walletContext}>
                    <ToastContainer theme="dark" />
                    <Wrapper>
                        <Component {...pageProps} />
                    </Wrapper>
                </WalletContext.Provider>
            </HackatonManagerContext.Provider>
        </HackatonManagerFactoryContext.Provider>
    )
}

export default MyApp
