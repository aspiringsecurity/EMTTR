import React, { useState } from "react"
import { useForm } from "react-hook-form"
import { toast } from "react-toastify"
import Input from "components/shared/Input"
import Button from "components/shared/Button"
import useHackatonManager, { Participant } from "utils/context/hackatonManagerContext"
import useWallet from "utils/context/walletContext"
import { extractRevertReason } from "utils/helpers"

const JoinHackaton: React.FC = () => {
    const [loading, setLoading] = useState<boolean>(false)
    const { registerParticipant } = useHackatonManager()

    const { connected } = useWallet()

    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm<Participant>()

    const onSubmit = async (participant: Participant) => {
        setLoading(true)
        try {
            await registerParticipant(participant)
            toast(`Joined to hackaton as ${participant.teamName}`)
        } catch (err) {
            console.log(err)
            const msg = extractRevertReason(err)
            console.log(err)
            toast.error(msg || err.message)
        }
        setLoading(false)
    }

    return (
        <form onSubmit={handleSubmit(onSubmit)}>
            <Input
                label="Team Name"
                placeholder="Team Name"
                error={errors.teamName && "Team name is required."}
                {...register("teamName", { required: true })}
            />
            <Input
                placeholder="Project Name"
                label="Project Name"
                error={errors.projectName && "Project name is required."}
                {...register("projectName", { required: true })}
            />
            <Input
                placeholder="Project Link"
                label="Project Link"
                error={errors.projectLink && "Project link is required."}
                {...register("projectLink", { required: true })}
            />
            <Button loading={loading} disabled={!connected || loading} className="mt-5">
                Submit
            </Button>
        </form>
    )
}
export default JoinHackaton
