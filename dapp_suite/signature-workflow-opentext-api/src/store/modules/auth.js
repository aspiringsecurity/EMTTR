export default {
  namespaced: true,
  state: {
    loginStatus: "",
    user: {
      accessToken: "",
      username: "",
      method: ""
    }
  },
  getters: {
    authBody: (state, getters, rootState, rootGetters) => {
      const systemInfo = rootGetters["systemInfo/systemInfo"];
      return {
        username: systemInfo.username,
        password: systemInfo.password,
        grant_type: "password",
        client_id: systemInfo.clientID,
        client_secret: systemInfo.clientSecret,
        subscription: systemInfo.subscription
      };
    },
    authHeader: state => ({
      Authorization: state.user.accessToken,
      "Content-Type": "application/json"
    }),
    user: state => state.user
  },
  mutations: {
    loginFailure(state, { error }) {
      state.loginStatus = error.body;
    },
    loginSuccess(state, { result }) {
      state.user.accessToken = `Bearer ${result.body.access_token}`;
      state.user.username = this.getters["systemInfo/systemInfo"].username;
      state.loginStatus = "success";
    }
  }
};
