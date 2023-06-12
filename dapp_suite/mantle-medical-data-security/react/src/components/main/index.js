import React from 'react'
import PropTypes from 'prop-types'
import { withStyle } from 'utils/styles'

const ContentWrapper = withStyle({ margin: '0 auto' })('div')

const Main = ({ style, children }) => {
  const Wrapper = withStyle(({ theme }) => ({
    position: 'relative',
    display: 'block',
    minWidth: 800,
    width: '100vw',
    height: '100vh',
    overflow: 'auto',
    padding: theme.spacing.unit * 4,
    ...style
  }))('main')

  return (
    <Wrapper>
      <ContentWrapper>{children}</ContentWrapper>
    </Wrapper>
  )
}

Main.propTypes = {
  children: PropTypes.node.isRequired
}

export default Main
