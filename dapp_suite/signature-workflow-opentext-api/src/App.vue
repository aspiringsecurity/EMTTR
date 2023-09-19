<template>
  <div id="app">
    <img alt="Core Signature logo" src="./assets/logo.png" />
    <Welcome />
    <LoginTabs />
    <Separator />
    <DocumentUpload />
    <Separator />
    <SignatureRequestTabs />
    <Separator />
    <DocumentPolling />
  </div>
</template>

<script>
import DocumentPolling from "./components/DocumentPolling";
import DocumentUpload from "./components/DocumentUpload";
import LoginTabs from "./components/LoginTabs";
import Separator from "./components/Separator";
import SignatureRequestTabs from "./components/SignatureRequestTabs";
import Welcome from "./components/Welcome";

export default {
  name: "App",
  components: {
    DocumentPolling,
    DocumentUpload,
    LoginTabs,
    Separator,
    SignatureRequestTabs,
    Welcome
  }
};
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  width: 80%;
  margin: 25px 10% 0 10%;
}
div {
  text-align: left;
}
input {
  margin-bottom: 15px;
}
label {
  font-size: 0.8em;
  font-weight: 600;
}
.input-div-width {
  width: 30em;
}
.input-width {
  width: 100%;
  margin: 5px 0;
}
.success {
  color: #00d1b2;
}
.error {
  color: #d72727;
}
</style>
