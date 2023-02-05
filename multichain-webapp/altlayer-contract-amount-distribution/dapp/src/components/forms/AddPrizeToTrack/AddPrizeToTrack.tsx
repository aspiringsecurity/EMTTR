import React, { useState } from "react"
import { useForm } from "react-hook-form"
import { toast } from "react-toastify"
import Input from "components/shared/Input"
import Button from "components/shared/Button"
import useHackatonManager, { PriceForTrack } from "utils/context/hackatonManagerContext"
import useWallet from "utils/context/walletContext"
import { ethers } from "ethers"
import { extractRevertReason } from "utils/helpers"

const AddPrizeToTrack: React.FC = () => {
    const [loading, setLoading] = useState<boolean>(false)
    const { hackatonState, addPrizeToTrack } = useHackatonManager()

    const { connected } = useWallet()

    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm()

    const onSubmit = async (data) => {
        setLoading(true)
        const addPrice: PriceForTrack = {
            trackName: data.trackName,
            prizeAmount: data.prizeAmount,
            prizeName: data.prizeName,
        }

        try {
            await addPrizeToTrack(addPrice)

            toast(
                `${addPrice.prizeName} added to track ${addPrice.trackName} for a whopping: ${addPrice.prizeAmount} ETH`
            )
        } catch (err) {
            const msg = extractRevertReason(err)

            console.log(err)
            toast.error(msg || err.message)
        }

        setLoading(false)
    }
    const renderTracks = () =>
        hackatonState.tracks.length ? (
            <div>
                <div className="grid grid-cols-1 gap-4">
                    {hackatonState.tracks.map((track) => (
                        <div key={track.name} className="col-span-2 mt-2">
                            <h3 className="mt-1 font-bold text-gray-200">{track.name}</h3>
                            <p className="mt-1 text-gray-400">
                                {ethers.utils.formatEther(track.poolAmount)} ETH
                            </p>
                            <form onSubmit={handleSubmit(onSubmit)}>
                                <div className="flex justify-between items-end">
                                    <div className="flex">
                                        <input
                                            {...register("trackName")}
                                            value={track.name}
                                            type="hidden"
                                        />
                                        <Input
                                            placeholder={`Prize Name`}
                                            label={`Prize Name`}
                                            error={errors.prizeName && "Prize name is required."}
                                            {...register(`prizeName`, { required: true })}
                                        />
                                        <Input
                                            placeholder={`Prize`}
                                            label={`Prize (ETH)`}
                                            error={errors.prizeAmount && "Prize is required."}
                                            className="ml-3"
                                            {...register(`prizeAmount`, { required: true })}
                                        />
                                    </div>
                                </div>

                                <div className="flex justify-between">
                                    <Button
                                        loading={loading}
                                        disabled={!connected || loading}
                                        className="mt-5"
                                        onClick={() => handleSubmit(onSubmit)}
                                    >
                                        Add Prize
                                    </Button>
                                </div>
                            </form>
                        </div>
                    ))}
                </div>
            </div>
        ) : null

    const renderForms = () => {
        if (hackatonState.funded) {
            return <div className="col-span-3 bg-zinc-800 py-5">{renderTracks()}</div>
        }
        return null
    }

    return <div className="container mx-auto">{renderForms()}</div>
}
export default AddPrizeToTrack
