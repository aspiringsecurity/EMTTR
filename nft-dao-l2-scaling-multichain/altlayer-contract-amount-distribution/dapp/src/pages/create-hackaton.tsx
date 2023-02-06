import type { NextPage } from "next"
import CreateHackaton from "components/CreateHackaton"
import Head from "next/head"

const CreateHackatonPage: NextPage = () => {
    return (
        <>
            <Head>
                <title>Create Hackaton</title>
            </Head>
            <CreateHackaton />
        </>
    )
}

export default CreateHackatonPage
