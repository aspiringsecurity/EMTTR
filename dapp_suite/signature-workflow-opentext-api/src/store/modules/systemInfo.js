export default {
  namespaced: true,
  state: {
    systemInfo: {}
  },
  getters: {
    systemInfo: state => state.systemInfo
  },
  mutations: {
    setSystemInfo(state, systemInfo) {
      state.systemInfo = systemInfo;
    }
  }
};
