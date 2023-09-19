<template>
  <div>
    <div class="tabs">
      <ul>
        <li
          v-for="tab in tabs"
          :key="tab.id"
          :class="{ 'is-active': tab.isActive }"
        >
          <a :href="tab.href" @click="selectTab(tab)">{{ tab.name }}</a>
        </li>
      </ul>
    </div>

    <div class="tabs-details">
      <slot></slot>
    </div>
  </div>
</template>

<script>
import { mapState } from "vuex";

export default {
  name: "Tabs",
  data() {
    return {
      tabs: []
    };
  },
  computed: {
    ...mapState("auth", ["user"])
  },
  created() {
    this.tabs = this.$children;
    this.user.method = "ot2";
  },
  methods: {
    selectTab(selectedTab) {
      this.tabs.forEach(tab => {
        tab.isActive = tab.name === selectedTab.name;
      });
      this.user.method = selectedTab.method;
    }
  }
};
</script>

<style scoped>
li.is-active a {
  color: #00d1b2;
}
a {
  color: #777;
  cursor: pointer;
  text-decoration: none;
}
ul {
  list-style: none;
}
li {
  display: inline-block;
  padding: 10px 20px;
  border-bottom: 1px solid #999;
}
li.is-active {
  border-bottom-color: #00d1b2;
}
</style>
