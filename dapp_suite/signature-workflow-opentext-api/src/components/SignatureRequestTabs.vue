<template>
  <div class="container">
    <tabs>
      <tab
        name="Only Others Signature Request"
        method="onlyOthers"
        :selected="true"
      >
        <SignatureRequestOnlyOthers />
      </tab>
      <tab name="Only Me Signature Request" method="onlyMe">
        <SignatureRequestOnlyMe />
      </tab>
    </tabs>
  </div>
</template>

<script>
import tab from "./tab";
import tabs from "./tabs";
import SignatureRequestOnlyOthers from "./SignatureRequestOnlyOthers";
import SignatureRequestOnlyMe from "./SignatureRequestOnlyMe";

export default {
  name: "SignatureRequestTabs",
  components: {
    SignatureRequestOnlyOthers,
    SignatureRequestOnlyMe,
    tab,
    tabs
  }
};
</script>

<style scoped>
.container {
  position: relative;
}
</style>
