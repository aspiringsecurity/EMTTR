import Vue from "vue";
import Vuex from "vuex";

import auth from "@/store/modules/auth";
import events from "@/store/modules/events";
import documents from "@/store/modules/documents";
import systemInfo from "@/store/modules/systemInfo";
import signatureRequests from "@/store/modules/signatureRequests";

Vue.use(Vuex);

const store = new Vuex.Store({
  state: {},
  mutations: {},
  actions: {},
  modules: {
    auth,
    events,
    documents,
    systemInfo,
    signatureRequests
  }
});

export { store };
