import React, { useState } from "react"
import { useForm } from "react-hook-form"
import { toast } from "react-toastify"
import Input from "components/shared/Input"
import Button from "components/shared/Button"
import useHackatonManager, { Participant } from "utils/context/hackatonManagerContext"
import useWallet from "utils/context/walletContext"
import { extractRevertReason } from "utils/helpers"

const SubmitProject: React.FC = () => {
    const [loading, setLoading] = useState<boolean>(false)
    const { submitProject } = useHackatonManager()

    const { connected } = useWallet()

    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm<Participant>()

    const onSubmit = async (form: any) => {
        setLoading(true)
        try {
            await submitProject(form.teamName)
            toast(`Submitted project by ${form.teamName}`)
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
            <Button loading={loading} disabled={!connected || loading} className="mt-5">
                Submit
            </Button>
        </form>
    )
}
export default SubmitProject
