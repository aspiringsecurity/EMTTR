<template>
  <div class="grid gap-16 p-16 rounded-30 text-center border-1 border-grey-200">
    <img
      class="justify-self-center w-40 h-40 object-cover"
      :src="logo"
      :alt="name"
    >
    <div class="grid gap-6">
      <h2 class="typo-title-l">
        {{ name }}
      </h2>
      <div
        v-if="propsSocials"
        class="grid grid-flow-col justify-center"
      >
        <a
          v-if="propsSocials.discord"
          :href="getSocialUrl(['discord.gg', 'discord.com/invite'], propsSocials.discord)"
          rel="nofollow noopener noreferrer"
          target="_blank"
          class="grid place-items-center w-20 h-20 text-info"
        >
          <UtilsIcon
            name="Logo/Discord"
            class="w-full h-full"
          />
        </a>
        <a
          v-if="propsSocials.github"
          :href="getSocialUrl('github.com', propsSocials.github)"
          rel="nofollow noopener noreferrer"
          target="_blank"
          class="grid place-items-center w-20 h-20 text-info"
        >
          <UtilsIcon
            name="Logo/Github"
            class="w-full h-full"
          />
        </a>
        <a
          v-if="propsSocials.twitter"
          :href="getSocialUrl('twitter.com', propsSocials.twitter)"
          rel="nofollow noopener noreferrer"
          target="_blank"
          class="grid place-items-center w-20 h-20 text-info"
        >
          <UtilsIcon
            name="Logo/Twitter"
            class="w-full h-full"
          />
        </a>
        <a
          v-if="propsSocials.telegram"
          :href="getSocialUrl('t.me', propsSocials.telegram)"
          rel="nofollow noopener noreferrer"
          target="_blank"
          class="grid place-items-center w-20 h-20 text-info"
        >
          <UtilsIcon
            name="Logo/Telegram"
            class="w-full h-full"
          />
        </a>
        <a
          v-if="propsSocials.website"
          :href="propsSocials.website"
          rel="nofollow noopener noreferrer"
          target="_blank"
          class="grid place-items-center w-20 h-20 text-info"
        >
          <UtilsIcon
            name="Logo/Website"
            class="w-full h-full"
          />
        </a>
      </div>
    </div>
    <a
      :href="link"
      rel="nofollow noopener noreferrer"
      target="_blank"
      class="grid place-items-center self-end h-40 typo-button-s rounded-50 border-1 border-grey-200 transition-border duration-100 hover:border-grey-100"
    >
      Launch
    </a>
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

const props = defineProps<Governance>()

const propsSocials = computed<Governance['socials'] | null>(() => props.socials || null)

const getSocialUrl = (pattern: string | string[], social: string): string => {
  if (Array.isArray(pattern)) {
    return pattern.find((checkedPattern) => social.includes(checkedPattern)) ? social : `https://${pattern[0]}/${social}`
  }
  return social.includes(pattern) ? social : `https://${pattern}/${social}`
}
</script>
