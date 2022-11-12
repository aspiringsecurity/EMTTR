import React, { Component } from 'react';
import { Route } from 'react-router-dom';

const _renderMergedProps = (component, ...rest) => {
  const finalProps = Object.assign({}, ...rest);
  return (
    React.createElement(component, finalProps)
  );
}

/**
 * Renders a @external "Route" with props passed to the route
 * and available to all components mounted in that route
 * 
 * @param {React.Component} component - component to render for the route
 * @param {params} ...rest - props to pass to component when it's mounted
 */
const PropsRoute = ({ component, ...rest }) => {
  return (
    <Route {...rest} render={routeProps => {
      return _renderMergedProps(component, routeProps, rest);
    }} />
  );
}

export default PropsRoute;
