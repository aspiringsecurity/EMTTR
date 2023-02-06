import React from "react"

interface Props {
    label?: string
    type?: string
    error?: string
    className?: string
    options: [{ name: string; value: string }]
}

const Input: React.FC<Props> = React.forwardRef(
    ({ label, placeholder, error, className, options, ...props }, ref) => {
        return (
            <>
                {label ? (
                    <label
                        htmlFor={props.id}
                        className="block mb-2 text-sm font-medium text-gray-900 dark:text-gray-400"
                    >
                        {label}
                    </label>
                ) : null}
                <select
                    className={`bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500 ${
                        className || ""
                    }`}
                    {...props}
                    ref={ref}
                >
                    {options.map((option) => (
                        <option key={option.value} value={option.value}>
                            {option.name}
                        </option>
                    ))}
                </select>
                {error && <p className="text-red-500 absolute bottom-0">{error}</p>}
            </>
        )
    }
)
export default Input
