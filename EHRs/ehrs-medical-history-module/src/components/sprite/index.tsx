import { rem } from '../../utils'
import sprite from './icons.svg'

interface SpriteProps {
  height: number
  width: number
  id: string
  style?: React.CSSProperties
  className?: string
  onClick?: () => void
}

const Sprite = ({
  height,
  width,
  style,
  className,
  id,
  onClick,
}: SpriteProps) => {
  return (
    <svg
      height={rem(height)}
      width={rem(width)}
      viewBox={`0 0 ${width} ${height}`}
      style={{ ...style }}
      className={className || ''}
      onClick={onClick}
    >
      <use href={`${sprite}#${id}`} />
    </svg>
  )
}

export default Sprite
