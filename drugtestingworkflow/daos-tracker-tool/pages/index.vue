<template>
  <div>
    <div
      class="relative grid pt-60 before:content-[''] before:absolute before:left-0 before:top-0 before:right-0 before:bottom-0 before:pointer-events-none"
      :class="$style.hero"
    >
      <Container class="grid place-items-center">
        <h1 class="typo-title-l text-center">
          Meet the autonomous organizations <span class="text-primary">of the ICON ecosystem</span>
        </h1>
        <nuxt-link
          to="/deploy"
          class="grid place-items-center self-center px-40 w-120 h-40 typo-button-s rounded-50 border-1 border-grey-200 transition-border duration-100 hover:border-grey-100"
        >
          Launch your DAO
        </nuxt-link>
      </Container>
    </div>
    <Container class="my-40">
      <div
        v-if="isLoading"
        class="grid place-items-center text-primary"
      >
        <UtilsLoader />
      </div>
      <div
        v-else-if="hasError"
        class="px-12 py-8 typo-paragraph text-error bg-error bg-opacity-20 rounded-5"
      >
        Something wrong happened. Please retry later.
      </div>
      <div
        v-else-if="!governances.length"
        class="px-12 py-8 typo-paragraph text-info bg-info bg-opacity-20 rounded-5"
      >
        No governance has been found.
      </div>
      <div
        v-else
        class="grid gap-20 s:grid-cols-2 m:grid-cols-3 l:grid-cols-4 xl:grid-cols-4 xxl:grid-cols-4"
      >
        <DisplaysCardGovernance
          v-for="(governance, i) in governances"
          :key="`governance-${i}`"
          v-bind="governance"
        />
      </div>
    </Container>
  </div>
</template>

<script setup lang="ts">
type Governance = {
  name: string
  link: string
  logo: string
  socials?: {
    discord?: string
    github?: string
    twitter?: string
    telegram?: string
    website?: string
  }
}

const isLoading = ref<boolean>(true)
const governances = ref<Governance[]>([])
const hasError = ref<boolean>(false)

onMounted(async () => {
  try {
    if (process.env.NODE_ENV === 'development') {
      const modules = import.meta.glob('~/assets/data/*.json')
      const data = await Promise.all(Object.entries(modules).filter(([file]) => !file.includes('/example.json')).map(([file, module]) => module())).then<Governance[]>((result) => result.map((module) => module.default))
      governances.value = data.sort((a, b) => (a.name > b.name ? 1 : -1))
    } else {
      const r = require.context('~/assets/data', true, /\.json$/)
      const data = r.keys().map<Governance>((key) => r(key))
      governances.value = data.sort((a, b) => (a.name > b.name ? 1 : -1))
    }
  } catch (error) {
    hasError.value = true
  } finally {
    isLoading.value = false
  }
})
</script>

<style lang="scss" module>
.hero {
  height: 210px;
  background: radial-gradient(ellipse at -50px -75px, #0890FE, #231A3B 75%);

  &::before {
    background-image: url("~/assets/images/waves.svg");
    background-repeat: no-repeat;
    background-position: center;
    background-size: cover;
  }
}
</style>
