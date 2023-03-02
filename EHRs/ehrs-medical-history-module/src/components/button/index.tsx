import { CSSProperties, ReactNode } from 'react'
import './button.scss'

interface ButtonProps
  extends React.DetailedHTMLProps<
    React.ButtonHTMLAttributes<HTMLButtonElement>,
    HTMLButtonElement
  > {
  children: ReactNode
  className?: string
  style?: CSSProperties
  onClick?: () => void
  buttonType?: 'default' | 'alternate'
}

const Button = ({
  children,
  className,
  style,
  onClick,
  buttonType,
}: ButtonProps) => {
  return (
    <button
      className={`button ${buttonType === 'alternate' ? buttonType : ''} ${
        className || ''
      }`}
      onClick={onClick}
      style={{ ...style }}
    >
      {children}
    </button>
  )
}

export default Button
