import React, { useMemo, useState } from "react"
import { useForm } from "react-hook-form"
import { toast } from "react-toastify"
import Button from "components/shared/Button"
import useHackatonManager, { Participant } from "utils/context/hackatonManagerContext"
import useWallet from "utils/context/walletContext"
import Select from "components/shared/Select"
import { extractRevertReason } from "utils/helpers"

const SetWinner: React.FC = () => {
    const [loading, setLoading] = useState<boolean>(false)
    const { setWinner, hackatonState } = useHackatonManager()

    const tracksOptions = useMemo(
        () => hackatonState.tracks.map((track) => ({ name: track.name, value: track.name })),
        [hackatonState.tracks]
    )

    const participantsOptions = useMemo(
        () =>
            hackatonState.participants.map((participant) => ({
                name: participant._teamName,
                value: participant._teamName,
            })),
        [hackatonState.participants]
    )

    const { connected } = useWallet()

    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm()

    const onSubmit = async (form: any) => {
        setLoading(true)
        try {
            await setWinner(form.trackName, "prize", form.teamName)
            toast(`Set a winner ${form.teamName} for track ${form.trackName}`)
        } catch (err) {
            const msg = extractRevertReason(err)
            console.log(err)
            toast.error(msg || err.message)
        }
        setLoading(false)
    }

    return (
        <form onSubmit={handleSubmit(onSubmit)}>
            <div className="grid grid-cols-4 gap-4">
                <div className="col-span-2">
                    <Select
                        label="Select a track"
                        options={tracksOptions}
                        {...register("trackName", { required: true })}
                        error={errors.teamName && "Team name is required"}
                    />
                </div>
                <div className="col-span-2">
                    <Select
                        label="Select a team"
                        options={participantsOptions}
                        {...register("teamName", { required: true })}
                        error={errors.teamName && "Team name is required"}
                    />
                </div>
            </div>

            <Button loading={loading} disabled={!connected || loading} className="mt-5">
                Submit
            </Button>
        </form>
    )
}
export default SetWinner

// import { useForm } from "react-hook-form"

// // The following component is an example of your existing Input Component
// const Input = ({ label, register, required }) => (
//     <>
//         <label>{label}</label>
//         <input {...register(label, { required })} />
//     </>
// )

// // you can use React.forwardRef to pass the ref too
// const Select = React.forwardRef(({ onChange, onBlur, name, label }, ref) => (
//     <>
//         <label>{label}</label>
//         <select name={name} ref={ref} onChange={onChange} onBlur={onBlur}>
//             <option selected>Selec1t</option>
//             <option value="20">20</option>
//             <option value="30">30</option>
//         </select>
//     </>
// ))

// const App = () => {
//     const { register, handleSubmit } = useForm()

//     const onSubmit = (data) => {
//         alert(JSON.stringify(data))
//     }

//     return (
//         <form onSubmit={handleSubmit(onSubmit)}>
//             <Input label="First Name" register={register} required />
//             <Select label="Age" {...register("Age")} />
//             <input type="submit" />
//         </form>
//     )
// }

// export default App
