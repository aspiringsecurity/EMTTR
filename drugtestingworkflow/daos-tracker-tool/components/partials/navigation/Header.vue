<template>
  <header class="fixed top-0 z-100 grid items-center w-full h-60 pointer-events-none">
    <Container class="grid gap-20 grid-flow-col items-center justify-between">
      <nuxt-link
        to="/"
        class="grid gap-10 grid-flow-col items-center pointer-events-auto"
      >
        <div class="grid place-items-center w-20 h-20 rounded-full bg-white">
          <UtilsIcon
            name="Logo/Icon"
            class="w-12 h-12 text-[#1BBDC2]"
          />
        </div>
        <div class="text-14 font-bold">
          ICON DAOs
        </div>
      </nuxt-link>
      <div class="grid gap-10 grid-flow-col items-center pointer-events-auto">
        <a
          v-for="(link, i) in links"
          :key="`link-${i}`"
          :href="link.url"
          rel="nofollow noopener noreferrer"
          target="_blank"
          class="typo-button-l opacity-60 transition-opacity duration-100 hover:opacity-100"
        >
          {{ link.text }}
        </a>
      </div>
    </Container>
  </header>
</template>

<script setup lang="ts">
type Link = { text: string, url: string }

const links = ref<Link[]>([
  { text: 'Deploy tutorial', url: 'https://medium.com/staky-io/how-to-set-up-a-dao-on-icon-145cd47de1cf/' },
  { text: 'ICON website', url: 'https://icon.community/' },
  { text: 'ICON docs', url: 'https://docs.icon.community' },
  { text: 'Why $ICX?', url: 'https://whyicx.com/' },
])
</script>
