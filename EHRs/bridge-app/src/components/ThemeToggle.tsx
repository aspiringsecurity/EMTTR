import { RiMoonLine, RiSunLine } from '@remixicon/react'

import { Button } from '@eth-optimism/ui-components'

import { useTheme } from '@/providers/ThemeProvider'
import { useCallback } from 'react'

const classNames = {
  light:
    'h-[1.2rem] w-[1.2rem] rotate-0 scale-100 transition-all dark:-rotate-90 dark:scale-0',
  dark: 'absolute h-[1.2rem] w-[1.2rem] rotate-90 scale-0 transition-all dark:rotate-0 dark:scale-100',
}

export const ThemeToggle = () => {
  const { theme, setTheme } = useTheme()

  const onThemeToggle = useCallback(() => {
    setTheme(theme === 'dark' ? 'light' : 'dark')
  }, [theme, setTheme])

  const themeToggle =
    theme === 'dark' ? (
      <RiMoonLine className={classNames.dark} />
    ) : (
      <RiSunLine className={classNames.light} />
    )

  return (
    <Button
      className="self-center"
      variant={'ghost'}
      size="icon"
      onClick={onThemeToggle}
    >
      {themeToggle}
    </Button>
  )
}
