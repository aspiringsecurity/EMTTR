import React, { useState } from "react"
import { useForm, useFieldArray } from "react-hook-form"
import { toast } from "react-toastify"
import Input from "components/shared/Input"
import Button from "components/shared/Button"
import useWallet from "utils/context/walletContext"
import useHackatonManager, { Track } from "utils/context/hackatonManagerContext"
import { extractRevertReason } from "utils/helpers"

const HackatonMainData: React.FC = () => {
    const [loading, setLoading] = useState<boolean>(false)
    const { createTracks } = useHackatonManager()

    const { connected } = useWallet()

    const {
        control,
        register,
        handleSubmit,
        formState: { errors },
    } = useForm({
        defaultValues: {
            tracks: [{ trackName: "", trackPrize: "" }],
        },
    })
    const { fields, append, remove } = useFieldArray({
        control,
        name: "tracks",
    })

    const onSubmit = async ({ tracks }: { tracks: Track[] }) => {
        setLoading(true)
        for (const track of tracks) {
            try {
                await createTracks(track)
                toast(`Track ${track.trackName} added`)
            } catch (err) {
                const msg = extractRevertReason(err)

                console.log(err)
                toast.error(msg || err.message)
            }
        }
        setLoading(false)
    }

    return (
        <form onSubmit={handleSubmit(onSubmit)}>
            {fields.map((item, index) => (
                <div key={item.id} className="flex justify-between items-end">
                    <div className="flex">
                        <Input
                            placeholder={`Track ${index + 1} Name`}
                            label={`Track ${index + 1} Name`}
                            error={
                                errors.tracks &&
                                errors.tracks[index] &&
                                errors.tracks[index].trackName &&
                                "Track name is required."
                            }
                            {...register(`tracks[${index}].trackName`, { required: true })}
                        />
                        <Input
                            placeholder={`Track ${index + 1} Prize`}
                            label={`Track ${index + 1} Prize (ETH)`}
                            error={
                                errors.tracks &&
                                errors.tracks[index] &&
                                errors.tracks[index].trackPrize &&
                                "Track prize is required."
                            }
                            className="ml-3"
                            {...register(`tracks[${index}].trackPrize`, { required: true })}
                        />
                    </div>
                    {!!index && (
                        <Button className="mb-6" onClick={() => remove(index)}>
                            Delete
                        </Button>
                    )}
                </div>
            ))}

            <div className="flex justify-between">
                <Button
                    className="mt-5"
                    onClick={() => {
                        append({ trackName: "", trackPrize: "" })
                    }}
                >
                    + Add track
                </Button>
                <Button
                    loading={loading}
                    disabled={!connected || loading}
                    className="mt-5"
                    onClick={() => handleSubmit(onSubmit)}
                >
                    Submit tracks
                </Button>
            </div>
        </form>
    )
}
export default HackatonMainData
