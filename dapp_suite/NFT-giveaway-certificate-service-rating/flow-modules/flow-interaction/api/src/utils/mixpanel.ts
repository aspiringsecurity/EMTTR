import Mixpanel from "mixpanel";

let memoMixpanel;
export function mixpanelTrack(...args) {
  if (process.env.MIXPANEL_TOKEN) {
    if (!memoMixpanel) {
      memoMixpanel = Mixpanel.init(process.env.MIXPANEL_TOKEN);
    }
    memoMixpanel.track(...args);
  }
}
