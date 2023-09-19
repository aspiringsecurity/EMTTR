import Vue from "vue";

export default {
  namespaced: true,
  state: {
    signatureRequestsEndpoint: "/api/v1/signature-requests/",
    signatureRequestUUID: "",
    requestBody: {},
    canSendSignatureRequest: false
  },
  getters: {
    signatureRequestURL: (state, getters, rootState, rootGetters) =>
      `${rootGetters["systemInfo/systemInfo"].apiHost}${state.signatureRequestsEndpoint}`
  },
  mutations: {
    setSignatureRequestUUID(state, UUID) {
      state.signatureRequestUUID = UUID;
    },
    setRequestBody(state, requestBody) {
      state.requestBody = requestBody;
    },
    setCanSendSignatureRequest(state, canSendSignatureRequest) {
      state.canSendSignatureRequest = canSendSignatureRequest;
    }
  },
  actions: {
    async sendSignatureRequest({ getters, commit, rootGetters }, { body }) {
      const headers = rootGetters["auth/authHeader"];

      return new Promise((resolve, reject) => {
        Vue.http({
          method: "POST",
          url: getters.signatureRequestURL,
          body,
          headers
        })
          .then(result => {
            commit(
              "signatureRequests/setSignatureRequestUUID",
              result.body.uuid,
              { root: true }
            );
            resolve(result);
          })
          .catch(error => {
            console.error(error);
            reject(error);
          });
      });
    }
  }
};
