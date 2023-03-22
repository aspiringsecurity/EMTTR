const plugin = require('tailwindcss/plugin')

const shareEntries = (keys, values) => keys
  .reduce((accu, curr) => ({ ...accu, [curr]: values }), {})

const fractions = (number) => [...new Array(number)]
  .reduce((accu, _, index) => ({ ...accu, [`${index + 1}fr`]: `${index + 1}fr` }), {})

const combineRules = (rules, length) => [...new Array(length)]
  .reduce((result, _, index) => ({
    ...result,
    ...Object.entries(rules)
      .map((entry) => [...new Array(index)]
        .reduce((accu) => accu
          .map(([currentKey, currentValue]) => Object.entries(rules)
            .map(([subKey, subValue]) => [
              `${currentKey}-${subKey}`,
              `${currentValue} ${subValue}`,
            ]))
          .flat(), [entry]))
      .flat()
      .reduce((accu, [key, value]) => ({ ...accu, [key]: value }), {}),
  }), {})

module.exports = {
  theme: {
    // Tailwind Utility : xs:, s, etc.
    screens: {
      xxs: '360px',
      xs: '472px',
      s: '640px',
      m: '768px',
      l: '1024px',
      xl: '1280px',
      xxl: '1680px',
    },
    // Tailwind Utility : bg-, text- etc.
    colors: {
      none: 'none',
      transparent: 'transparent',
      current: 'currentColor',
      inherit: 'inherit',

      black: '#111111',
      white: '#F8FAFC',

      primary: '#3BCFEF',

      grey: {
        default: '#889DBC',
        100: '#889DBC',
        200: '#3A3D5D',
        300: '#252846',
        400: '#1C1F3E',
      },

      success: '#52D5BA',
      warning: '#FFAF2A',
      error: '#FF4267',
      info: '#0890FE',
    },
    // Tailwind Utility : p-, m- etc.
    spacing: {
      none: 'none',
      auto: 'auto',
      full: '100%',

      // Rem
      // +1
      0: '0.0rem',
      1: '0.1rem',
      2: '0.2rem',
      // +2
      4: '0.4rem',
      6: '0.6rem',
      8: '0.8rem',
      10: '1.0rem',
      12: '1.2rem',
      // +4
      16: '1.6rem',
      20: '2.0rem',
      24: '2.4rem',
      // +8
      32: '3.2rem',
      40: '4.0rem',
      48: '4.8rem',
      // +12
      60: '6.0rem',
      72: '7.2rem',
      84: '8.4rem',
      96: '9.6rem',
      // +16
      112: '11.2rem',
      128: '12.8rem',
      144: '14.4rem',
      160: '16.0rem',
      // +32
      192: '19.2rem',
      224: '22.4rem',
      256: '25.6rem',
      288: '28.8rem',
      320: '32.0rem',
      // +64
      384: '38.4rem',
      448: '44.8rem',
      512: '51.2rem',
      576: '57.6rem',
      640: '64.0rem',
      // +128
      768: '76.8rem',
      896: '89.6rem',
      960: '96.0rem',
      1024: '102.4rem',
      1152: '115.2rem',
      1280: '128.0rem',

      // Percentages
      '1/2': '50%',
      '1/3': '33.333333%',
      '2/3': '66.666667%',
      '1/4': '25%',
      '2/4': '50%',
      '3/4': '75%',
      '1/5': '20%',
      '2/5': '40%',
      '3/5': '60%',
      '4/5': '80%',
      '1/6': '16.666667%',
      '2/6': '33.333333%',
      '3/6': '50%',
      '4/6': '66.666667%',
      '5/6': '83.333333%',
      '1/12': '8.333333%',
      '2/12': '16.666667%',
      '3/12': '25%',
      '4/12': '33.333333%',
      '5/12': '41.666667%',
      '6/12': '50%',
      '7/12': '58.333333%',
      '8/12': '66.666667%',
      '9/12': '75%',
      '10/12': '83.333333%',
      '11/12': '91.666667%',
    },
    space: (theme, { negative }) => ({
      ...theme('spacing'),
      ...negative(theme('spacing')),
    }),
    inset: (theme, { negative }) => ({
      ...theme('spacing'),
      ...negative(theme('spacing')),
    }),
    // Tailwind Utility : z-
    zIndex: {
      auto: 'auto',
      '-1': '-1',
      0: '0',
      1: '1',
      10: '10',
      20: '20',
      30: '30',
      40: '40',
      50: '50',
      60: '60',
      70: '70',
      80: '80',
      90: '90',
      100: '100',
    },
    // Tailwind Utility : min-w-
    minWidth: (theme, { breakpoints }) => ({
      ...theme('spacing'),
      ...breakpoints(theme('screens')),
    }),
    // Tailwind Utility : w-
    width: (theme) => theme('spacing'),
    // Tailwind Utility : max-w-
    maxWidth: (theme, { breakpoints }) => ({
      ...theme('spacing'),
      ...breakpoints(theme('screens')),
    }),
    // Tailwind Utility : min-h-
    minHeight: (theme) => theme('spacing'),
    // Tailwind Utility : h-
    height: (theme) => theme('spacing'),
    // Tailwind Utility : max-h-
    maxHeight: (theme) => theme('spacing'),
    // Tailwind Utility : p-
    padding: (theme) => theme('spacing'),
    // Tailwind Utility : m-
    margin: (theme, { negative }) => ({
      ...theme('spacing'),
      ...negative(theme('spacing')),
    }),
    // Tailwind Utility : font-
    fontFamily: {
      inter: ['Inter', '-apple-system', 'BlinkMacSystemFont', '"Segoe UI"', 'Roboto', '"Helvetica Neue"', 'sans-serif'],
      sans: ['system-ui', '-apple-system', 'BlinkMacSystemFont', '"Segoe UI"', 'Roboto', '"Helvetica Neue"', 'sans-serif'],
      serif: ['Georgia', 'Cambria', '"Times New Roman"', 'Times', 'serif'],
      mono: ['Menlo', 'Monaco', 'Consolas', '"Liberation Mono"', '"Courier New"', 'monospace'],
    },
    // Tailwind Utility : text-
    fontSize: {
      0: '0.0rem',
      10: '1.0rem',
      12: '1.2rem',
      14: '1.4rem',
      16: '1.6rem',
      18: '1.8rem',
      20: '2.0rem',
      24: '2.4rem',
      32: '3.2rem',
      40: '4.0rem',
      48: '4.8rem',
      60: '6.0rem',
      72: '7.2rem',
      84: '8.4rem',
      96: '9.6rem',
    },
    // Tailwind Utility : font-
    fontWeight: {
      thin: '100',
      extralight: '200',
      light: '300',
      regular: '400',
      medium: '500',
      semibold: '600',
      bold: '700',
      extrabold: '800',
      black: '900',
    },
    // Tailwind Utility : tracking-
    letterSpacing: {
      0: '0.0px',
      1: '1.0px',
      2: '2.0px',
    },
    // Tailwind Utility : leading-
    lineHeight: {
      none: '1',
      tight: '1.25',
      regular: '1.5',
      wide: '1.75',
      double: '2',
    },
    // Tailwind Utility : border-
    borderColor: (theme) => ({
      ...theme('colors'),
      default: theme('colors.black', 'currentColor'),
    }),
    // Tailwind Utility : rounded-
    borderRadius: {
      inherit: 'inherit',
      default: '5.0px',
      full: '100%',
      max: '9999px',
      0: '0.0px',
      5: '5.0px',
      10: '10.0px',
      20: '20.0px',
      30: '30.0px',
      50: '50.0px',
    },
    // Tailwind Utility : border-, border-t- etc.
    borderWidth: {
      default: '1px',
      0: '0px',
      1: '1px',
      2: '2px',
      4: '4px',
      6: '6px',
      8: '8px',
      10: '10px',
    },
    // Tailwind Utility : opacity-
    opacity: {
      inherit: 'inherit',
      0: '0.00',
      5: '0.05',
      10: '0.10',
      15: '0.15',
      20: '0.20',
      25: '0.25',
      30: '0.30',
      35: '0.35',
      40: '0.40',
      45: '0.45',
      50: '0.50',
      55: '0.55',
      60: '0.60',
      65: '0.65',
      70: '0.70',
      75: '0.75',
      80: '0.80',
      85: '0.85',
      90: '0.90',
      95: '0.95',
      100: '0.99',
    },
    // Tailwind Utility : transform & scale-
    scale: {
      0: '0',
      50: '.5',
      75: '.75',
      875: '.875',
      100: '1',
      112.5: '1.125',
      125: '1.25',
      150: '1.5',
    },
    // Tailwind Utility : transform & rotate-, -rotate- etc.
    rotate: {
      '-180': '-180deg',
      '-135': '-135deg',
      '-90': '-90deg',
      '-45': '-45deg',
      0: '0',
      45: '45deg',
      90: '90deg',
      135: '135deg',
      180: '180deg',
    },
    // Tailwind Utility : transform & skew-, -skew- etc.
    skew: {
      '-12': '-12deg',
      '-6': '-6deg',
      '-3': '-3deg',
      0: '0',
      3: '3deg',
      6: '6deg',
      12: '12deg',
    },
    // Tailwind Utility : transform & translate-, -translate- etc.
    translate: (theme, { negative }) => ({
      ...theme('spacing'),
      ...negative(theme('spacing')),
      '-full': '-100%',
      '-1/2': '-50%',
    }),
    // Tailwind Utility : transition-
    transitionProperty: {
      all: 'all',
      none: 'none',
      default: 'color, background-color, border-color, border-opacity, opacity, transform',
      width: 'width',
      height: 'height',
      size: 'width, height',
      margin: 'margin',
      padding: 'padding',
      spacing: 'margin, padding',
      color: 'color',
      background: 'background-color',
      border: 'border-color, border-opacity',
      layout: 'width, height, margin, padding, background-color, border-color, border-opacity',
      opacity: 'opacity',
      transform: 'transform',
      composite: 'opacity, transform',
    },
    // Tailwind Utility : ease-
    transitionTimingFunction: {
      default: 'cubic-bezier(.25, .1, .25, 1)',
      linear: 'linear',
      'reveal-xxs': 'cubic-bezier(.5, 0, 0, .25)',
      'reveal-xs': 'cubic-bezier(.5, 0, 0, .5)',
      'reveal-s': 'cubic-bezier(.5, 0, 0, .75)',
      'reveal-ms': 'cubic-bezier(.5, 0, 0, .875)',
      'reveal-m': 'cubic-bezier(.5, 0, 0, 1)',
      'reveal-ml': 'cubic-bezier(.5, 0, 0, 1.125)',
      'reveal-l': 'cubic-bezier(.5, 0, 0, 1.25)',
      'reveal-xl': 'cubic-bezier(.5, 0, 0, 1.5)',
      'reveal-xxl': 'cubic-bezier(.5, 0, 0, 2)',
      // in: 'cubic-bezier(0.4, 0, 1, 1)',
      // out: 'cubic-bezier(0, 0, 0.2, 1)',
      // 'in-sine': 'cubic-bezier(0.47, 0, 0.745, 0.715)',
      // 'in-quad': 'cubic-bezier(0.55, 0.085, 0.68, 0.53)',
      // 'in-cubic': 'cubic-bezier(0.55, 0.055, 0.675, 0.19)',
      // 'in-quart': 'cubic-bezier(0.895, 0.03, 0.685, 0.22)',
      // 'in-quint': 'cubic-bezier(0.755, 0.05, 0.855, 0.06)',
      // 'in-expo': 'cubic-bezier(0.95, 0.05, 0.795, 0.035)',
      // 'in-circ': 'cubic-bezier(0.6, 0.04, 0.98, 0.335)',
      // 'in-back': 'cubic-bezier(0.6, -0.28, 0.735, 0.045)',
      // 'out-sine': 'cubic-bezier(0.39, 0.575, 0.565, 1)',
      // 'out-quad': 'cubic-bezier(0.25, 0.46, 0.45, 0.94)',
      // 'out-cubic': 'cubic-bezier(0.215, 0.61, 0.355, 1)',
      // 'out-quart': 'cubic-bezier(0.165, 0.84, 0.44, 1)',
      // 'out-quint': 'cubic-bezier(0.23, 1, 0.32, 1)',
      // 'out-expo': 'cubic-bezier(0.19, 1, 0.22, 1)',
      // 'out-circ': 'cubic-bezier(0.075, 0.82, 0.165, 1)',
      // 'out-back': 'cubic-bezier(0.175, 0.885, 0.32, 1.275)',
      // 'in-out': 'cubic-bezier(0.4, 0, 0.2, 1)',
      // 'in-out-sine': 'cubic-bezier(0.445, 0.05, 0.55, 0.95)',
      // 'in-out-quad': 'cubic-bezier(0.455, 0.03, 0.515, 0.955)',
      // 'in-out-cubic': 'cubic-bezier(0.645, 0.045, 0.355, 1)',
      // 'in-out-quart': 'cubic-bezier(0.77, 0, 0.175, 1)',
      // 'in-out-quint': 'cubic-bezier(0.86, 0, 0.07, 1)',
      // 'in-out-expo': 'cubic-bezier(1, 0, 0, 1)',
      // 'in-out-circ': 'cubic-bezier(0.785, 0.135, 0.15, 0.86)',
      // 'in-out-back': 'cubic-bezier(0.68, -0.55, 0.265, 1.55)',
    },
    // Tailwind Utility : duration-
    transitionDuration: {
      0: '0ms',
      100: '100ms',
      200: '200ms',
      250: '250ms',
      300: '300ms',
      400: '400ms',
      500: '500ms',
      750: '750ms',
      1000: '1000ms',
      2000: '2000ms',
      5000: '5000ms',
    },
    // Tailwind Utility : delay-
    transitionDelay: (theme) => ({
      ...theme('transitionDuration'),
    }),
    extend: {
      // Tailwind Utility : grid-{rows|cols}-
      ...shareEntries([
        'gridTemplateRows', // grid-rows-
        'gridTemplateColumns', // grid-cols-
      ], (theme) => combineRules({
        ...Object.entries(theme('spacing'))
          .filter(([key]) => [
            'auto',
            '1/2',
          ].includes(key))
          .reduce((accu, [key, value]) => ({ ...accu, [key]: value }), {}),
        ...fractions(3),
      }, 3)),
      // Tailwind Utility : will-change-
      willChange: {
        opacity: 'opacity',
        composite: 'opacity, transform',
      },
    },
  },
  variants: {
    order: ['responsive'],
    width: ['responsive', 'hover', 'group-hover'],
    height: ['responsive', 'hover', 'group-hover'],
    margin: ['responsive', 'first', 'last'],
    padding: ['responsive', 'first', 'last'],
    overflow: ['responsive', 'hover'],
    textColor: ['hover', 'focus', 'dark', 'dark-hover', 'dark-active', 'dark-placeholder', 'group-hover'],
    placeholderColor: ['dark', 'focus'],
    gradientColorStops: ['hover', 'group-hover'],
    backgroundColor: ['responsive', 'hover', 'focus', 'dark', 'dark-hover', 'dark-group-hover', 'dark-even', 'dark-odd'],
    backgroundOpacity: ['responsive', 'hover', 'focus', 'dark', 'dark-hover', 'dark-group-hover', 'dark-even', 'dark-odd'],
    borderColor: ['responsive', 'hover', 'focus', 'dark', 'dark-focus', 'dark-focus-within'],
    borderWidth: ['hover'],
    transformOrigin: ['responsive'],
    transform: ['responsive', 'hover', 'focus', 'active', 'group-hover'],
    scale: ['responsive', 'hover', 'focus', 'active', 'group-hover'],
    opacity: ['responsive', 'hover', 'focus', 'active', 'group-hover'],
    translate: ['responsive', 'hover', 'focus', 'active', 'group-hover'],
  },
  plugins: [
    plugin(({ addUtilities }) => {
      addUtilities({
        '.h-screen': { height: '100vh' },
        '.w-screen': { width: '100vw' },
        '.min-h-screen': { 'min-height': '100vh' },
        '.min-w-screen': { 'min-width': '100vw' },
        '.max-h-screen': { 'max-height': '100vh' },
        '.max-w-screen': { 'max-width': '100vw' },
      }, ['responsive'])
    }),
  ],
  content: [
    './components/**/*.{vue,js,ts}',
    './components/*.{vue,js,ts}',
    './layouts/**/*.vue',
    './layouts/*.vue',
    './pages/**/*.vue',
    './pages/*.vue',
    './plugins/**/*.{js,ts}',
    './plugins/*.{js,ts}',
    './app.vue',
  ],
}
