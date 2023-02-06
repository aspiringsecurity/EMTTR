import extract from "extract-json-from-string"

const truncateRegex = /^(0x[a-zA-Z0-9]{4})[a-zA-Z0-9]+([a-zA-Z0-9]{4})$/

export const truncateEthAddress = (address: string) => {
    const match = address.match(truncateRegex)
    if (!match) return address
    return `${match[1]}…${match[2]}`
}

export const extractRevertReason = (err) => {
    const extractedJSON = extract(err.message)
    let msg = ""
    extractedJSON.forEach((obj) => {
        if (obj.message) {
            msg = obj.message
        }
    })
    return msg
}
