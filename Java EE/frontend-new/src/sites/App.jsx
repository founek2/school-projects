import React, { Component } from 'react'
import { BrowserRouter as Router, Route, Link, Switch } from 'react-router-dom'
import Login from './Login';
import Main from './Main'
import UserManagement from './UserManagement';
import { withStyles } from '@material-ui/core/styles'
import Typography from '@material-ui/core/Typography'
import Menu from "../components/Menu"
import { getItem, delItem } from '../utils/storage';
import { useHistory } from "react-router-dom"
import * as loginAPI from "../api/login"

const styles = {
    root: {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: "column"
    },
    userName: {
        position: 'absolute',
        left: 10,
        top: 4
    },
    mainMenu: {
        position: 'absolute',
        right: 10,
        top: 5
    },
}
const Layout = withStyles(styles)(function ({ classes, children }) {
    const user = JSON.parse(getItem("user"))
    const history = useHistory()
    async function logOut() {
        await loginAPI.signOut()
        delItem('jwt')
        delItem('user')
        history.push('/')
    }

    return (<div className={classes.root}>
        <Typography className={classes.userName}>Logged as: {user.userName}</Typography>
        {/* <IconButton onClick={this.logOut} className={classes.exitButton}>
                      <ExitToAppIcon fontSize="default" />
            </IconButton> */}
        <Menu className={classes.mainMenu} onLogout={logOut} user={user} />
        {children}
    </div>
    )
})

class App extends Component {
    render() {
        return (
            <Router>
                <Switch>
                    <Route path="/user_management" render={() => <UserManagement layout={Layout} />} />
                    <Route path="/app" render={() => <Main layout={Layout} />} />
                    <Route path="/" component={Login} />
                </Switch>
            </Router>
        )
    }
}

export default App
