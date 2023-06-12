import React from 'react'
import PropTypes from 'prop-types'
import { isFunction } from 'lodash'
import { withStyles as withMuiStyles } from '@material-ui/core/styles'
import hoistNonReactStatics from 'hoist-non-react-statics'
import { wrapDisplayName } from 'recompose'

/**
 [1]. @NOTE: Using React.memo() does not show this Display name in React
 devtools currently (tested with React v16.7.0).
 */

/* :: (object || Function) -> Function -> Function */
export const withStyles = stylesOrCreator => BaseComponent => {
  const isStaticStyles = !isFunction(stylesOrCreator)
  if (isStaticStyles) {
    return withMuiStyles(stylesOrCreator)(BaseComponent)
  }

  const stylesCreator = stylesOrCreator
  const WithStyles = props =>
    React.createElement(
      withMuiStyles(theme => stylesCreator({ ...props, theme }))(BaseComponent),
      props
    )

  hoistNonReactStatics(WithStyles, BaseComponent)

  if (process.env.NODE_ENV !== 'production') {
    WithStyles.displayName = wrapDisplayName(
      BaseComponent,
      '$WithStyles'
    ) /* [1] */
  }

  return React.memo(WithStyles)
}

/* :: (object || Function) -> (string || Function) -> Function */
export const withStyle = styleOrCreator => BaseComponent => {
  const stylesOrCreator = isFunction(styleOrCreator)
    ? (...args) => ({ root: styleOrCreator(...args) })
    : { root: styleOrCreator }

  const WithStyle = ({ classes, className, ...rest }) => (
    <BaseComponent className={`${classes.root} ${className}`} {...rest} />
  )

  WithStyle.defaultProps = {
    className: ''
  }

  WithStyle.propTypes = {
    classes: PropTypes.shape({
      root: PropTypes.string.isRequired
    }).isRequired
  }

  hoistNonReactStatics(WithStyle, BaseComponent)

  if (process.env.NODE_ENV !== 'production') {
    WithStyle.displayName = wrapDisplayName(BaseComponent, 'WithStyle')
  }

  return withStyles(stylesOrCreator)(WithStyle)
}
