<template>
  <div class="container">
    <tabs>
      <tab name="Login with OT2" method="ot2" :selected="true">
        <OT2Login />
      </tab>
    </tabs>
  </div>
</template>

<script>
import tab from "./tab";
import tabs from "./tabs";
import OT2Login from "./OT2Login";

export default {
  name: "LoginTabs",
  components: {
    OT2Login,
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
