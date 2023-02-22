import React from 'react';
import ReactDOM from 'react-dom';
import Menu from './Menu';

it('Menu renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Menu />, div);
});