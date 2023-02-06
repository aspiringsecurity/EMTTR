import React, { useState } from "react"
import { useForm } from "react-hook-form"
import { toast } from "react-toastify"
import Input from "components/shared/Input"
import Button from "components/shared/Button"
import useHackatonManager from "utils/context/hackatonManagerContext"
import useWallet from "utils/context/walletContext"
import { extractRevertReason } from "utils/helpers"

const FundHacakton: React.FC = () => {
    const [loading, setLoading] = useState<boolean>(false)
    const { fundHackaton } = useHackatonManager()
    const { connected } = useWallet()

    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm()

    const onSubmit = async (data: any) => {
        setLoading(true)
        try {
            await fundHackaton(data.fundAmount)
            toast(`Hackaton Funded with ${data.fundAmount} ETH`)
        } catch (err) {
            const msg = extractRevertReason(err)
            toast.error(msg || err.message)
        }
        setLoading(false)
    }

    return (
        <form onSubmit={handleSubmit(onSubmit)}>
            <Input
                placeholder="Fund hackaton with ETH"
                label="Fund amount "
                error={errors.fundAmount && "Funding hackaton is required."}
                {...register("fundAmount", {
                    required: true,
                })}
            />
            <Button loading={loading} disabled={!connected || loading} className="mt-5">
                Fund
            </Button>
        </form>
    )
}
export default FundHacakton
