import React, { Component, useState } from 'react'
import logo from '../assets/images/logo.svg'
import * as usersAPI from '../api/users'
import * as registrationApi from '../api/registration'
import * as loginApi from '../api/login'
import Button from '@material-ui/core/Button'
import TextField from '@material-ui/core/TextField'
import { withStyles } from '@material-ui/core/styles'
import Paper from '@material-ui/core/Paper'
import List from '@material-ui/core/List'
import ListItem from '@material-ui/core/ListItem'
import ListItemIcon from '@material-ui/core/ListItemIcon'
import ListItemText from '@material-ui/core/ListItemText'
import ListSubheader from '@material-ui/core/ListSubheader'
import Divider from '@material-ui/core/Divider'
import Typography from '@material-ui/core/Typography'
import { getItem, storeItem } from '../utils/storage'

const styles = theme => ({
    window: {
        width: 400,
        padding: 38,
        paddingLeft: 60,
        paddingRight: 60
    },
    windowHeader: {
        textAlign: 'center'
    },
    root: {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center'
    },
    actions: {
        paddingTop: 10,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center'
    },

    userList: {
        maxHeight: 500,
        overflowY: 'scroll',
        width: 300
    },
    userItem: {
        paddingTop: 0,
        paddingBottom: 0
    }
})

class Login extends Component {
    state = {
        userName: '',
        passwd: '',
        allUsers: []
    }
    handleChange = key => e => this.setState({ [key]: e.target.value })

    resetInputs = () => this.setState({ userName: '', passwd: '' })

    fetchAllUsers = () => {
        usersAPI.fetchAll().then(array => this.setState({ allUsers: array }))
    }

    componentDidMount() {
        this.fetchAllUsers()
    }

    createUser = () => {
        const { userName, passwd } = this.state
        registrationApi
            .createUser({ userName, passwd })
            .then(res => {
                if (res.status === 400) {
                    return res.json().then(body => {
                        let str = ''
                        for (const viol of body.parameterViolations) {
                            str += viol.path + ' ' + viol.message + '\n'
                        }
                        alert('Validation violated\n' + str)
                    })
                }
            })
            .then(this.resetInputs)
            // .then(() => alert('User created'))
            .then(this.fetchAllUsers)
            .catch(e => {
                if (e != 1) alert('User already exists')
                console.log('ERROR> ' + 'problem in Registration', e)
            })
    }

    signIn = () => {
        const { passwd, userName } = this.state
        loginApi
            .signIn({ userName, passwd })
            .then(user => {
                console.log('user> ', user)
                storeItem('jwt', user.token.jwt)
                storeItem('user', JSON.stringify({ id: user.id, privileged: user.privileged, userName: user.userName }))
                this.props.history.push('/app')
            })
            .catch(e => {
                console.log(e)
                if (e === 403) alert('Wrong password or username')
            })
    }

    render() {
        const { passwd, userName, allUsers } = this.state
        const { classes } = this.props

        return (
            <div>
                <div className={classes.root}>
                    <Paper className={classes.window}>
                        <Typography variant="h4" className={classes.windowHeader}>
                            TODO App
                              </Typography>
                        <TextField
                            label="Name"
                            className={classes.textField}
                            value={userName}
                            onChange={this.handleChange('userName')}
                            margin="normal"
                            fullWidth
                        />
                        <TextField
                            label="password"
                            className={classes.textField}
                            value={passwd}
                            onChange={this.handleChange('passwd')}
                            margin="normal"
                            type="password"
                            fullWidth
                        />
                        <div className={classes.actions}>
                            <Button color="secondary" onClick={this.signIn}>
                                Sign In
                                   </Button>
                            <Button color="primary" onClick={this.createUser}>
                                Create
                                   </Button>
                        </div>
                    </Paper>
                </div>
                <List
                    component="nav"
                    aria-label="Users"
                    className={classes.userList}
                    subheader={
                        <ListSubheader component="div" id="nested-list-subheader">
                            Registered users:
                              </ListSubheader>
                    }
                >
                    <Divider />
                    {allUsers.map(u => (
                        <ListItem button key={u.id} className={classes.userItem}>
                            <ListItemText primary={'- ' + u.userName} />
                        </ListItem>
                    ))}
                </List>
            </div>
        )
    }
}

export default withStyles(styles)(Login)
