<template>
  <div>
    <p>
      <em
        >Note: Obtaining an access token should be done in your backend. This
        login form is for demonstration purposes only.</em
      >
    </p>
    <label for="systemFile"
      >Add system information from a json file. This step is optional if you
      want to manually enter values in the form below.</label
    >
    <div class="input-div-width">
      <input id="systemFile" type="file" @change="processFile($event)" />
    </div>
    <div class="input-div-width">
      <label for="authHost">Authentication Host</label>
      <input
        id="authHost"
        v-model="systemInfo.authHost"
        class="input-width"
        type="text"
        placeholder="Enter your authentication host (e.g https://otdsauth.ot2.opentext.com)"
      />
      <label for="apiHost">Signature API Host</label>
      <input
        id="apiHost"
        v-model="systemInfo.apiHost"
        class="input-width"
        type="text"
        placeholder="Enter your api host (e.g  http://signature.demo:5000/"
      />
      <label for="site">Site</label>
      <input
        id="site"
        v-model="systemInfo.site"
        class="input-width"
        type="text"
        placeholder="Enter your site (e.g 1234-1234-1234-1234-1234)"
      />
      <label for="clientID">Client ID</label>
      <input
        id="clientID"
        v-model="systemInfo.clientID"
        class="input-width"
        type="text"
        placeholder="Enter your site's client ID"
      />
      <label for="ot2ClientSecret">Client Secret</label>
      <input
        id="ot2ClientSecret"
        v-model="systemInfo.clientSecret"
        class="input-width"
        type="password"
        placeholder="Enter your site's client secret"
      />
      <label for="ot2Subscription">Subscription</label>
      <input
        id="ot2Subscription"
        v-model="systemInfo.subscription"
        class="input-width"
        type="text"
        placeholder="Enter your subscription"
      />
      <label for="ot2Username">Username</label>
      <input
        id="ot2Username"
        v-model="systemInfo.username"
        class="input-width"
        type="email"
        placeholder="Enter your username (e.g signer@opentext.com)"
      />
      <label for="ot2Password">Password</label>
      <input
        id="ot2Password"
        v-model="systemInfo.password"
        class="input-width"
        type="password"
        placeholder="Enter your password"
      />
    </div>
  </div>
</template>

<script>
import { mapGetters, mapMutations } from "vuex";

export default {
  name: "SystemInfo",
  computed: {
    ...mapGetters("systemInfo", ["systemInfo"])
  },
  methods: {
    ...mapMutations("systemInfo", ["setSystemInfo"]),
    async processFile(event) {
      const file = event.target.files[0];
      const fileAsJSON = await this.toJSON(file);
      const systemInfo = {};
      systemInfo.site = fileAsJSON.site;
      systemInfo.clientID = fileAsJSON.clientID;
      systemInfo.clientSecret = fileAsJSON.clientSecret;
      systemInfo.subscription = fileAsJSON.subscription;
      systemInfo.username = fileAsJSON.username;
      systemInfo.password = fileAsJSON.password;
      systemInfo.authHost = fileAsJSON.authHost;
      systemInfo.apiHost = fileAsJSON.apiHost;
      this.setSystemInfo(systemInfo);
    },
    toJSON(file) {
      return new Promise((resolve, reject) => {
        const reader = new FileReader();
        reader.readAsText(file);
        reader.onload = () => resolve(JSON.parse(reader.result));
        reader.onerror = error => reject(error);
      });
    }
  }
};
</script>
