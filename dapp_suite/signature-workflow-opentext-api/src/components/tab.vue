<template>
  <div v-if="isActive"><slot></slot></div>
</template>

<script>
export default {
  name: "Tab",
  props: {
    name: { type: String, required: true },
    selected: { type: Boolean, default: false },
    method: { type: String, required: true }
  },
  data() {
    return {
      isActive: false
    };
  },
  computed: {
    href() {
      return `#${this.name.toLowerCase().replace(/ /g, "-")}`;
    }
  },
  mounted() {
    this.isActive = this.selected;
  }
};
</script>
