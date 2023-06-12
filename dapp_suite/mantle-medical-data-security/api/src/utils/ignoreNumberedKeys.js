/**
 * Contract method call responses seem to include numbered keys in the response object
 * which are duplicates of the key:value pairs we're returning. These are not needed so
 * we extract them from the response.
 * @param {object} initialObject
 * @return {object}
 */
const ignoreNumberedKeys = (initialObject) => {
  const result = {}

  for (const key in initialObject) {
    if (isNaN(key)) {
      result[key] = initialObject[key]
    }
  }

  return result
}

module.exports = ignoreNumberedKeys
