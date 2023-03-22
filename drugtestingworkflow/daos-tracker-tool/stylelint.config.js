module.exports = {
  extends: [
    'stylelint-config-rational-order',
    'stylelint-config-airbnb',
  ],
  rules: {
    'at-rule-no-unknown': null,
    'max-nesting-depth': 3,
    'selector-max-id': 1,
    'at-rule-empty-line-before': [
      'always',
      {
        ignore: ['inside-block', 'blockless-after-same-name-blockless'],
      },
    ],
  },
}
