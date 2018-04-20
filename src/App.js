import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import Elm from 'react-elm-components'
import { Main } from './elm/src/Main'
class App extends Component {
  render() {
    return (
      <Elm src={Main} />
    );
  }
}

export default App;
