import React from 'react';
import { hot } from 'react-hot-loader/root';
import Window from './components/Window.jsx';
import withTheme from './containers/withTheme.jsx'

function App() {
    return <Window />;
}

export default hot(withTheme(App));
