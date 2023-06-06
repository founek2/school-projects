import React, { Component } from 'react'
import { BrowserRouter as Router, Route, Link, Switch } from 'react-router-dom'
import Login from './Login';
import Main from './Main'

class App extends Component {
     render() {
          return (
               <Router>
                    <Switch>
				<Route path="/app" component={Main}/>
					<Route path="/" component={Login}/>
				</Switch>
               </Router>
          )
     }
}

export default App
