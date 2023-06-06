import React from 'react'
import IconButton from '@material-ui/core/IconButton'
import Menu from '@material-ui/core/Menu'
import MenuItem from '@material-ui/core/MenuItem'
import MoreVertIcon from '@material-ui/icons/MoreVert'
import { delItem } from '../utils/storage';
import { o } from 'ramda'
import * as userApi from '../api/users'
import { withRouter } from "react-router-dom"

function MyMenu({ className, onLogout, user, history }) {
    const [anchorEl, setAnchorEl] = React.useState(null)

    function handleClick(event) {
        setAnchorEl(event.currentTarget)
    }

    function handleClose() {
        setAnchorEl(null)
    }

    function handleDelete() {
        userApi.del([user.id]).then(res => {
            if (res.status >= 400) {
                alert("Something went wrong, try it later please")
                this.handleClose()
                throw 400
            }
        }).then(onLogout).catch(e => { })
    }

    return (
        <div className={className}>
            <IconButton aria-label="More" aria-controls="long-menu" aria-haspopup="true" onClick={handleClick}>
                <MoreVertIcon />
            </IconButton>
            <Menu id="simple-menu" anchorEl={anchorEl} keepMounted open={Boolean(anchorEl)} onClose={handleClose}>
                <MenuItem onClick={() => history.push("/user_management")}>Správa uživatelů</MenuItem>
                <MenuItem onClick={handleDelete}>Delete account</MenuItem>
                <MenuItem onClick={onLogout} >Sign Out</MenuItem>
            </Menu>
        </div>
    )
}

export default withRouter(MyMenu)
