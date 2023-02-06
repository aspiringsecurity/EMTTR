import type { NextPage } from "next"
import Head from "next/head"
import ListHackaton from "components/ListHackaton"

const ListHackatonPage: NextPage = () => {
    return (
        <>
            <Head>
                <title>List Hackatons</title>
            </Head>
            <ListHackaton />
        </>
    )
}

export default ListHackatonPage