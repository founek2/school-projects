import React, { Component, useState } from 'react'
import EnchancedTable from './Table'
import * as usersApi from '../api/users'
import { withStyles } from '@material-ui/styles'
import { getItem } from '../utils/storage'
import { o, propEq, not, filter, map, flip, contains, prop, compose, find, assocPath, assoc } from 'ramda'
import TextField from '@material-ui/core/TextField'
import Dialog from '@material-ui/core/Dialog'
import DialogActions from '@material-ui/core/DialogActions'
import DialogContent from '@material-ui/core/DialogContent'
import DialogContentText from '@material-ui/core/DialogContentText'
import DialogTitle from '@material-ui/core/DialogTitle'
import FormControlLabel from '@material-ui/core/FormControlLabel'
import Checkbox from '@material-ui/core/Checkbox'
import Button from '@material-ui/core/Button'

const convertToUsers = map(id => ({ id }))

const styles = theme => ({})
class UserOverview extends Component {
    constructor(props) {
        super(props)
        this.state = {
            users: [],
            selected: [],
            dialog: {
                open: false,
                user: {}
            }
        }
    }

    componentDidMount() {
        this.fetchAll()
    }

    fetchAll = () => {
        const { userName } = this.props.user
        usersApi.fetchAll().then((array) =>
            this.setState({
                users: filter(o(not, propEq('userName', userName)), array)
            })
        )
    }

    handleDelete = () => {
        const { selected, users } = this.state

        usersApi.del(convertToUsers(selected)).then(() => {
            const notInSelected = compose(
                not,
                flip(contains)(selected),
                prop('id')
            )

            this.setState({ selected: [], users: filter(notInSelected, users) })
        })
    }
    handleSelect = ({ target: { value } }) => {
        this.setState({ selected: value })
    }

    handleOpenDialog = id => {
        this.setState({
            dialog: {
                open: true,
                user: find(propEq('id', id), this.state.users)
            }
        })
    }

    closeDialog = () => {
        this.setState({ dialog: assoc('open', false, this.state.dialog) })
    }

    changeUserProp = key => val => {
        this.setState({ dialog: assocPath(['user', key], val, this.state.dialog) })
    }

    updateUser = () => {
        const { user } = this.state.dialog
        if (user.password == '') delete user.password

        usersApi.update(user).then(res => {
            if (res.status == 400) {
                alert("Validation violated\nPassword must have 4 <= len <= 40")
                throw 400
            }

        }).then(o(this.closeDialog, this.fetchAll)).catch(e => { })
    }

    render() {
        const { dialog, users } = this.state
        const { className } = this.props;
        return (
            <div className={className}>
                <EnchancedTable
                    dataProps={[
                        { key: 'userName', label: 'Uživ. jméno' },
                        { key: 'id', label: 'id' },
                        { key: 'privileged', label: 'admin', convertor: val => (val == true ? 'Ano' : 'Ne') }
                    ]}
                    data={users}
                    toolbarHead="Správa uživatelů"
                    onDelete={this.handleDelete}
                    orderBy="userName"
                    onChange={this.handleSelect}
                    // enableCreation={false}
                    // onAdd={() => this.updateCreateForm({ open: true })}
                    enableEdit={true}
                    onEdit={this.handleOpenDialog}
                    rowsPerPage={5}
                />
                <Dialog open={dialog.open} onClose={this.closeDialog} aria-labelledby="form-dialog-title">
                    <DialogTitle id="form-dialog-title">Subscribe</DialogTitle>
                    <DialogContent>
                        <DialogContentText>You can modify user's privileges or password.</DialogContentText>
                        <FormControlLabel
                            control={
                                <Checkbox
                                    checked={dialog.user.privileged}
                                    onChange={e => this.changeUserProp('privileged')(e.target.checked)}
                                />
                            }
                            label="Administrator"
                            className={''}
                        />
                        <TextField
                            autoFocus
                            margin="dense"
                            id="password"
                            label="Password"
                            type="password"
                            fullWidth
                            onChange={e => this.changeUserProp('passwd')(e.target.value)}
                        />
                    </DialogContent>
                    <DialogActions>
                        <Button onClick={this.closeDialog} color="primary">
                            Cancel
                              </Button>
                        <Button onClick={this.updateUser} color="primary">
                            Save
                              </Button>
                    </DialogActions>
                </Dialog>
            </div>
        )
    }
}

export default withStyles(styles)(UserOverview)
