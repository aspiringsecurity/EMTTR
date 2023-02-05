import { useState } from "react"
import Image from "next/image"

import { ethers, Signer } from "ethers"
import { truncateEthAddress } from "utils/helpers"
import Button from "components/shared/Button"
import useWallet from "utils/context/walletContext"
import useHackatonManagerFactory from "utils/context/hackatonManagerFactoryContext"
import useHackatonManager from "utils/context/hackatonManagerContext"
import Link from "next/link"

import Control from "../../../assets/control.png"
import Logo from "../../../assets/logo.png"
import Chart_fill from "../../../assets/Chart_fill.png"
import User from "../../../assets/User.png"
import Calendar from "../../../assets/Calendar.png"
import useMetamask from "hooks/use-metamask"

const SideBar = ({ children }) => {
    const [open, setOpen] = useState(true)

    const { wallet, handleConnectWallet, handleDisconnectWallet } = useMetamask()

    const Menus = [
        { title: "All Hackathons", src: Chart_fill, link: "/list-hackaton" },
        { title: "Create", src: Calendar, link: "/create-hackaton", gap: true },
    ]

    return (
        <div className="flex">
            <div
                className={` ${
                    open ? "w-72" : "w-20 "
                } bg-[#081A51] h-screen p-5  pt-8 relative duration-300 flex flex-col justify-between`}
            >
                <div>
                    <div
                        className={`flex flex-row justify-${
                            open ? "end" : "center"
                        } items-center mb-8`}
                    >
                        <Image
                            width={24}
                            height={24}
                            alt="icon"
                            src={Control}
                            className={`absolute cursor-pointer -right-3 top-9 w-7 border-dark-purple border-2 rounded-full  
                    ${!open && "rotate-180"}`}
                            onClick={() => setOpen(!open)}
                        />
                    </div>

                    <div className="flex gap-x-4 items-center">
                        <Image
                            width={34}
                            height={34}
                            alt="icon"
                            src={Logo}
                            className={`cursor-pointer duration-500 ${open && "rotate-[360deg]"}`}
                        />
                        <h1
                            className={`text-white origin-left font-medium text-xl duration-200 ${
                                !open && "scale-0"
                            }`}
                        >
                            User
                        </h1>
                    </div>
                    <ul className="pt-6">
                        {Menus.map((Menu, index) => (
                            <Link key={index} href={Menu?.link ?? ""}>
                                <li
                                    className={`flex  rounded-md p-2 cursor-pointer hover:bg-light-white text-gray-300 text-sm items-center gap-x-4 
                                ${Menu.gap ? "mt-9" : "mt-2"} ${index === 0 && "bg-light-white"} `}
                                >
                                    <Image width={30} height={30} alt="icon" src={Menu.src} />
                                    <span
                                        className={`${!open && "hidden"} origin-left duration-200`}
                                    >
                                        {Menu.title}
                                    </span>
                                </li>
                            </Link>
                        ))}
                    </ul>
                </div>
                {open ? (
                    <div>
                        <Button
                            className="w-full"
                            onClick={wallet.address ? handleDisconnectWallet : handleConnectWallet}
                        >
                            {wallet.address && wallet.balance ? (
                                <div className="w-full flex flex-row justify-between">
                                    <div>{truncateEthAddress(wallet.address)}</div>
                                    <div className="bg-blue-200 text-blue-800 px-2 font-bold rounded-r-md inline-flex items-center">
                                        {(+ethers.utils.formatEther(wallet.balance)).toFixed(2)} ETH
                                    </div>
                                </div>
                            ) : (
                                "Connect Wallet"
                            )}
                        </Button>
                    </div>
                ) : (
                    <li
                        className={`flex  rounded-md p-2 cursor-pointer hover:bg-light-white text-gray-300 text-sm items-center gap-x-4 
                               `}
                    >
                        <Image width={30} height={30} alt="icon" src={User} />
                    </li>
                )}
            </div>
            {/** Children */}
            <div className="h-screen flex-1 p-7 overflow-auto pb-24">{children}</div>
        </div>
    )
}
export default SideBar
