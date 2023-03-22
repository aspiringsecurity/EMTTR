type TruncateParams = {
  string: string
  start?: number
  end?: number
}

const truncateString = ({ string, start = 5, end = 5 }: TruncateParams): string => `${string.slice(0, start)}…${string.slice(string.length - end)}`

export const truncate = (args: string | TruncateParams): string => (typeof args === 'string' ? truncateString({ string: args }) : truncateString(args))
