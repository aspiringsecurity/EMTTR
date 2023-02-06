
// connectors.ts

import {UAuthMoralisConnector} from '@uauth/moralis'

// Instantiate your other connectors.
export const injected = {}

export const walletconnect = {provider: 'walletconnect'}

UAuthMoralisConnector.setUAuthOptions({
  clientID: import.meta.env.VITE_UNSTOPPABLE_CLIENT_ID,
  redirectUri: window.location.origin,

  // Scope must include openid and wallet
  scope: "openid wallet email",
  // Injected and walletconnect connectors are required
  connectors: { injected, walletconnect },
});

export const uauth = {
  connector: UAuthMoralisConnector,
  signingMessage: "Sign in to Tribunal",
};


const connectors = {
  injected,
  walletconnect,
  uauth,
}

export default connectors