import React from "react"

interface Props {
    children?: React.ReactNode
    onClick?: () => void
    className?: string
    loading?: boolean
    disabled?: boolean
}

const Button: React.FC<Props> = ({ children, loading, className, disabled, ...props }) => {
    return (
        <button
            {...props}
            disabled={disabled}
            className={`bg-blue-300 hover:bg-blue-400 text-blue-800 font-bold py-2 px-4 rounded-xl inline-flex items-center z-10 relative ${
                className || ""
            } ${disabled ? "opacity-50 cursor-not-allowed" : ""}`}
        >
            {loading ? (
                <svg
                    className="w-5 h-5 mr-3 -ml-1 text-white animate-spin"
                    xmlns="http://www.w3.org/2000/svg"
                    fill="none"
                    viewBox="0 0 24 24"
                >
                    <circle
                        className="opacity-25"
                        cx="12"
                        cy="12"
                        r="10"
                        stroke="currentColor"
                        strokeWidth="4"
                    ></circle>
                    <path
                        className="opacity-75"
                        fill="currentColor"
                        d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                    ></path>
                </svg>
            ) : null}
            {children}
        </button>
    )
}
export default Button