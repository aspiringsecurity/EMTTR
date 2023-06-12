import React from 'react'
import App from './App'

describe('<App />', () => {
  it('returns a valid React element', () => {
    const expected = true
    const actual = React.isValidElement(<App />)

    expect(actual).toEqual(expected)
  })
})
