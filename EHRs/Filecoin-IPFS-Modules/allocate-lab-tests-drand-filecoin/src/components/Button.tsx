import * as React from "react"

type ButtonProps = {
    onClick: () => void
    text: string
}

export const Button = (props: ButtonProps) =>
    <button
        id="encrypt-button"
        type="button"
        className="btn btn-primary"
        onClick={event => {
            event.preventDefault();
            props.onClick()
        }}
    >
        {props.text}
    </button>
