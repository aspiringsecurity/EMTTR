import type { NextPage } from "next"
import Dashboard from "components/Dashboard"
import Head from "next/head"

const HackatonPage: NextPage = () => {
    return (
        <>
            <Head>
                <title>Hackaton details</title>
            </Head>
            <Dashboard />
        </>
    )
}

export default HackatonPage
