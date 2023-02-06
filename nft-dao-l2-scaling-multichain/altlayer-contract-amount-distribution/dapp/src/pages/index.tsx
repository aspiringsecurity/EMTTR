import type { NextPage } from "next"
import Image from "next/image"
import BgImg from "../../assets/laptopBg.jpg"
import { useEffect } from "react"
import Button from "components/shared/Button"
import Link from "next/link"
import { FaLinkedinIn, FaTwitter } from "react-icons/fa"

const Home: NextPage = () => {
    useEffect(() => {
        var opacity = 0
        var intervalId: any
        const content = document.getElementById("textSection")!
        content.style.opacity = "0"

        const fadeIn = () => {
            intervalId = setInterval(appear, 50)
        }

        const appear = () => {
            opacity = Number(window.getComputedStyle(content).getPropertyValue("opacity"))

            if (opacity < 1) {
                opacity = opacity + 0.05
                content.style.opacity = String(opacity)
            } else {
                clearInterval(intervalId)
            }
        }

        setTimeout(() => {
            fadeIn()
        }, 2500)
    }, [])

    const teamMates = [
        {
            name: "Raushan Sharma",
            LinkedIn: "https://www.linkedin.com/in/raushansharma/",
            Twitter: "https://twitter.com/_raushansharma",
            textSize: "text-2xl",
        },
        {
            name: "Leon de Pruyssenaere de la Woestijne",
            LinkedIn: "https://www.linkedin.com/in/leonpw/",
            Twitter: "https://twitter.com/SlappyChuck",
            textSize: "text-lg",
        },
        {
            name: "Issa Kalonji",
            LinkedIn: "https://www.linkedin.com/in/issa-kalonji-b301851ba/",
            Twitter: "https://twitter.com/ISSDawg",
            textSize: "text-2xl",
        },
        {
            name: "Andrii Zahonenko",
            LinkedIn: "https://www.linkedin.com/in/andrii-zahonenko-23839515a/",
            // Twitter: "na",
            textSize: "text-2xl",
        },
        {
            name: "Aashir Sohail",
            LinkedIn: "https://www.linkedin.com/in/aashir-sohail/",
            Twitter: "https://twitter.com/AashirSohail6",
            textSize: "text-2xl",
        },
    ]

    return (
        <>
            <div
                style={{
                    position: "absolute",
                    height: "100vh",
                    width: "100%",
                    clipPath: "inset(0 0 0 0)",
                    zIndex: "-100",
                }}
            >
                <div
                    style={{
                        position: "fixed",
                        height: "100%",
                        width: "100%",
                        left: "0",
                        top: "0",
                    }}
                >
                    <Image src={BgImg} layout="fill" objectFit="cover" sizes="100vw" />
                </div>
            </div>

            {/* Main Content */}
            <section
                id="textSection"
                className="backdrop-blur-sm bg-black/70 backdrop-grayscale-[.5]"
            >
                <div className="h-[80vh] flex flex-col justify-center items-center ">
                    <h1 className="text-[7rem] font-semibold drop-shadow-md">
                        <span className="text-[#ff7f04]">Wen</span>
                        <span className="text-[#fb016e]">Bounty</span>
                    </h1>
                    <h2 className="text-xl">Real Time Hackathon Prize Distribution</h2>
                    <Link href="/create-hackaton">
                        <button
                            className="mt-10 px-10 py-4 bg-gradient-to-r from-[#ff7f04] to-[#fb016e] text-2xl hover:drop-shadow-lg rounded-lg"
                            onClick={() => null}
                        >
                            Launch App
                        </button>
                    </Link>
                </div>

                {/* About The Idea */}
                <div className="flex flex-col gap-10 justify-center bg-transparent text-center min-h-[20vh] w-2/3 mx-auto text-lg pb-36">
                    <h2 className="text-[#ff7f04] text-4xl font-semibold ">About The Project</h2>
                    <div>
                        Prize distribution in hackathons is extremely delayed and so tedious that it
                        can take months, and every winner gets frustrated. The organizing committee
                        is equally tired of constantly following up with the sponsors and updating
                        participants. While sponsors don't have a clear view on what's the optimum
                        amount they should spend in a hackathon.
                        <div className="py-2"></div>
                        Hence, WenBounty simplifies managing hackathons. Sponsors can add prize
                        money to a pool that auto disburses funds to winners. This will benefit
                        everyone in the following ways:
                    </div>
                    <div className="flex justify-between gap-5">
                        <div className="bg-black/90 rounded-lg px-5 py-16 border-2 border-[#fb016e]/50 hover:border-[#fb016e] flex-1">
                            <h3 className="text-2xl font-semibold">Participants</h3>
                            <p className="text-white/80 pt-5">
                                Clear view of which track has the highest prize; speedy & automated
                                prize distribution
                            </p>
                        </div>
                        <div className="bg-black/90 rounded-lg px-8 py-16 border-2 border-[#fb016e]/50 hover:border-[#fb016e] flex-1">
                            <h3 className="text-2xl font-semibold">Organizers</h3>
                            <p className="text-white/80 pt-5">
                                No headache of constantly following up with sponsors and updating
                                participants
                            </p>
                        </div>
                        <div className="bg-black/90 rounded-lg px-8 py-16 border-2 border-[#fb016e]/50 hover:border-[#fb016e] flex-1">
                            <h3 className="text-2xl font-semibold">Sponsors</h3>
                            <p className="text-white/80 pt-5">
                                Optimize prize money like an auction- start with a small fund & add
                                more basis participant responses.
                            </p>
                        </div>
                    </div>
                </div>

                {/* TEAM */}
                <div className="flex flex-col gap-10 justify-center bg-transparent text-center min-h-[20vh] w-2/3 mx-auto text-lg pb-36">
                    <h2 className="text-[#ff7f04] text-4xl font-semibold ">Our Team</h2>

                    <div className="flex justify-between gap-5">
                        {teamMates.map((eachMember) => (
                            <div
                                key={eachMember.name}
                                className="bg-black/90 rounded-lg px-5 py-16 border-2 border-[#fb016e]/50 hover:border-[#fb016e] flex-1"
                            >
                                <h3 className={`font-semibold ${eachMember.textSize}`}>
                                    {eachMember.name}
                                </h3>

                                <div className="flex gap-5 justify-center pt-5">
                                    {eachMember.Twitter ? (
                                        <a
                                            href={eachMember.Twitter}
                                            target="_blank"
                                            rel="noreferrer"
                                        >
                                            <FaTwitter />
                                        </a>
                                    ) : (
                                        ""
                                    )}

                                    {eachMember.LinkedIn ? (
                                        <a
                                            href={eachMember.LinkedIn}
                                            target="_blank"
                                            rel="noreferrer"
                                        >
                                            <FaLinkedinIn />
                                        </a>
                                    ) : (
                                        ""
                                    )}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </section>
        </>
    )
}

export default Home
