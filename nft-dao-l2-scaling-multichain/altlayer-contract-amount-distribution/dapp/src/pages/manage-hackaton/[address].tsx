import type { NextPage } from "next"
import HackatonDetails from "components/HackatonDetails"
import Head from "next/head"

const ManageHackatonPage: NextPage = () => {
    return (
        <>
            <Head>
                <title>Manage Hackaton</title>
            </Head>
            <HackatonDetails />
        </>
    )
}

export default ManageHackatonPage
