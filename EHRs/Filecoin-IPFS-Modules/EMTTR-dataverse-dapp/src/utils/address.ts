export function getAddressFromDid(did: string) {
  return did.slice(did.lastIndexOf(":") + 1);
}

export function getNamespaceAndReferenceFromDID(did: string) {
  const res = did.match("did:pkh:([-a-z0-9]{3,8}):([-_a-zA-Z0-9]{1,32})");
  if (!res) {
    throw new Error("Not in pkhDID format");
  }
  return {
    namespace: res[1],
    reference: res[2],
  };
}