import React from "react"

interface Props {
    label?: string
    placeholder?: string
    type?: string
    error?: string
    className?: string
}

const Input: React.FC<Props> = React.forwardRef(
    ({ label, placeholder, error, className, ...props }, ref) => {
        return (
            <div className="relative pb-6">
                {label ? (
                    <label
                        htmlFor={props.id}
                        className="block mb-2 text-sm font-medium text-gray-900 dark:text-gray-300"
                    >
                        {label}
                    </label>
                ) : null}

                <input
                    {...props}
                    className={`bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500 ${
                        className || ""
                    }`}
                    placeholder={placeholder}
                    ref={ref}
                />
                {error && <p className="text-red-500 absolute bottom-0">{error}</p>}
            </div>
        )
    }
)
export default Input
