const config = {
  extends: require.resolve('@appliedblockchain/eslint-config/react'),
  rules: {
    'no-mixed-operators': 0,
    'valid-jsdoc': 0,
    'func-style': [2, 'expression']
  },
  settings: {
    react: {
      version: '16.6.0' /* [1] */
    }
  }
}

module.exports = config
