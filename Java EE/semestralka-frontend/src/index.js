import React from 'react';
import ReactDOM from 'react-dom';
import { ThemeProvider } from '@material-ui/styles';
import CssBaseline from '@material-ui/core/CssBaseline';
import App from './sites/App';
import theme from './theme';
import './assets/styles/style.sass';
import './assets/styles/style.css';

ReactDOM.render(
	<ThemeProvider theme={theme}>
	{/* CssBaseline kickstart an elegant, consistent, and simple baseline to build upon. */}
	<CssBaseline />
	<App />
   </ThemeProvider>,
  document.getElementById('root'),
);

// Check if hot reloading is enable. If it is, changes won't reload the page.
// This is related to webpack-dev-server and works on development only.
if (module.hot) {
  module.hot.accept();
}
