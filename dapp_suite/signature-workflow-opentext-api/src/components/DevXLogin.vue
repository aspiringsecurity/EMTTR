<template>
  <div>
    <p>
      <em
        >Note: Obtaining an access token should be done in your backend. This
        login form is for demonstration purposes only.</em
      >
    </p>
    <div class="input-div-width">
      <label for="devxSite">Site</label>
      <input
        id="devxSite"
        v-model="systemInfo.site"
        class="input-width"
        type="text"
        placeholder="Enter your site (e.g 7a29b9c6-1026-44d2-85f6-9936bad079e6)"
      />
      <label for="devxClientID">Client ID</label>
      <input
        id="devxClientID"
        v-model="systemInfo.clientID"
        class="input-width"
        type="text"
        placeholder="Enter your site's client ID"
      />
      <label for="devxClientSecret">Client Secret</label>
      <input
        id="devxClientSecret"
        v-model="systemInfo.clientSecret"
        class="input-width"
        type="password"
        placeholder="Enter your site's client secret"
      />
      <label for="devxUsername">Username</label>
      <input
        id="devxUsername"
        v-model="systemInfo.username"
        class="input-width"
        type="email"
        placeholder="Enter your username (e.g signer@opentext.com)"
      />
      <label for="devxPassword">Password</label>
      <input
        id="devxPassword"
        v-model="systemInfo.password"
        class="input-width"
        type="password"
        placeholder="Enter your password"
      />
    </div>
    <button @click="loginToDevX">Login to OpenText Developer</button>
    <p v-if="loginStatus && loginStatus === 'success'" class="success">
      <strong
        >&#10003; Logged in successfully as {{ systemInfo.username }}!</strong
      >
    </p>
    <p v-if="loginStatus && loginStatus !== 'success'" class="error">
      <strong>Login failed: {{ loginStatus }}</strong>
    </p>
  </div>
</template>

<script>
import { mapState, mapGetters } from "vuex";

export default {
  name: "DevXLogin",
  computed: {
    ...mapGetters("auth", ["authBody"]),
    ...mapGetters("systemInfo", ["systemInfo"]),
    ...mapState("auth", ["loginStatus"]),
    authTokenUrl() {
      return `${this.systemInfo.authHost}/tenant/${this.systemInfo.site}/oauth2/token`;
    }
  },
  methods: {
    loginToDevX() {
      this.$http
        .post(this.authTokenUrl, this.authBody, {
          headers: { "Content-Type": "application/json" }
        })
        .then(result => {
          this.$store.commit("auth/loginSuccess", {
            result: result
          });
        })
        .catch(error => {
          this.$store.commit("auth/loginFailure", {
            error: error
          });
        });
    }
  }
};
</script>
