export const serializeQuery = (url: string, params: Record<string, string>) => (
  Object.keys(params).length
    ? `${url}?${new URLSearchParams(params).toString()}`
    : url
)
