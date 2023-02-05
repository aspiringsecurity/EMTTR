import React, { useState } from "react"
import { useForm } from "react-hook-form"
import { toast } from "react-toastify"
import Input from "components/shared/Input"
import Button from "components/shared/Button"
import useHackatonManagerFactory from "utils/context/hackatonManagerFactoryContext"
import useWallet from "utils/context/walletContext"
import { extractRevertReason } from "utils/helpers"

interface Props {
    setCreatedHackatonAddress: (address: string) => void
}

const CreateHackaton: React.FC<Props> = ({ setCreatedHackatonAddress }) => {
    const [loading, setLoading] = useState<boolean>(false)
    const { createNewHack } = useHackatonManagerFactory()
    const { connected } = useWallet()

    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm()

    const onSubmit = async (data: any) => {
        setLoading(true)
        try {
            const address = await createNewHack(data.name)
            toast(`Hackaton created at ${address}`)
            setCreatedHackatonAddress(address)
        } catch (err) {
            let msg = extractRevertReason(err)

            toast.error(msg || err.message)
        }
        setLoading(false)
    }

    return (
        <form onSubmit={handleSubmit(onSubmit)}>
            <Input
                placeholder="Hackaton Name"
                label="Hackaton Name"
                error={errors.name && "Hackaton name is required."}
                {...register("name", { required: true })}
            />
            <Button loading={loading} disabled={!connected || loading} className="mt-5">
                Create Hackaton
            </Button>
        </form>
    )
}
export default CreateHackaton
