import React from "react"
import Header from "components/Header"
import SideBar from "components/SideBar"

type Props = {
    children?: JSX.Element | JSX.Element[]
}

const Layout = ({ children }: Props) => {
    return (
        <div className="h-screen overflow-hidden">
            {/* <div className="py-2 top-0 z-50 bg-zinc-800">
                <Header />
            </div> */}
            <SideBar>{children}</SideBar>
        </div>
    )
}
export default Layout
