const path = require('path')

const isProduction = process.env.NODE_ENV === 'production'

module.exports = {
  root: true,
  env: {
    node: true,
    browser: true,
  },
  parser: 'vue-eslint-parser',
  parserOptions: {
    parser: '@typescript-eslint/parser',
  },
  plugins: [
    '@typescript-eslint',
  ],
  extends: [
    'plugin:vue/recommended',
    '@vue/airbnb',
    'plugin:@typescript-eslint/recommended',
  ],
  rules: {
    'no-console': isProduction ? 'error' : 'warn',
    'no-debugger': isProduction ? 'error' : 'warn',
    'no-undef': 0,
    'no-multiple-empty-lines': ['error', { max: 1, maxBOF: 1 }],
    'no-underscore-dangle': 'off',
    'no-param-reassign': 'off',
    'no-plusplus': 'off',
    'no-shadow': 'off',
    indent: 'off',
    quotes: ['error', 'single'],
    semi: ['error', 'never'],
    'global-require': 0,
    'linebreak-style': 0,
    'eslint linebreak-style': [0, 'error', 'windows'],
    'import/extensions': [
      'error',
      'ignorePackages',
      {
        js: 'never',
        jsx: 'never',
        mjs: 'never',
        ts: 'never',
        tsx: 'never',
      },
    ],
    'import/no-extraneous-dependencies': ['error', { devDependencies: true }],
    'import/prefer-default-export': 'off',
    'max-len': ['warn', {
      code: 250,
      ignoreComments: true,
      ignoreStrings: true,
      ignorePattern: 'd=([s]*?)',
    }],
    'object-curly-newline': [
      'error',
      [
        'ObjectExpression',
        'ObjectPattern',
        'ImportDeclaration',
        'ExportDeclaration',
      ].reduce((accu, curr) => ({
        ...accu,
        [curr]: {
          multiline: true,
          minProperties: 5,
          consistent: true,
        },
      }), {}),
    ],
    '@typescript-eslint/indent': ['error', 2, { SwitchCase: 1 }],
    '@typescript-eslint/no-duplicate-enum-values': 'error',
    '@typescript-eslint/no-shadow': ['error'],
    '@typescript-eslint/no-var-requires': 'off',
    'vue/html-indent': ['error', 2],
    'vue/require-default-prop': [1],
    'vue/valid-template-root': [0],
  },
  overrides: [
    {
      files: ['**/*.vue'],
      rules: {
        'no-unused-vars': 'off',
        '@typescript-eslint/no-unused-vars': 'off',
      },
    },
  ],
  ignorePatterns: [
    'assets/scripts/libs/**/*.js',
    'assets/scripts/libs/**/*.ts',
  ],
  globals: {
    $nuxt: true,
  },
  settings: {
    'import/resolver': {
      webpack: {
        config: {
          resolve: {
            alias: {
              '#app': path.join(__dirname, './node_modules/nuxt/dist/app'),
              '~': path.join(__dirname, '.'),
              '@': path.join(__dirname, '.'),
            },
            extensions: ['.js', '.jsx', '.ts', '.tsx', '.mjs'],
          },
        },
      },
    },
  },
}
