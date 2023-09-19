export default {
  namespaced: true,
  state: {
    documentsEndpoint: "/api/v1/documents/",
    documentUUID: ""
  },
  getters: {
    documentURL: state => `${state.documentsEndpoint}${state.documentUUID}/`,
    documentUUID: state => state.documentUUID
  },
  mutations: {
    setDocumentUUID(state, UUID) {
      state.documentUUID = UUID;
    }
  }
};
