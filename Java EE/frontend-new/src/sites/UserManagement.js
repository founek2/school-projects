import React, { useState } from "react"
import UserOverview from '../components/UserOverview'
import { withStyles } from '@material-ui/core/styles'
import { getItem } from '../utils/storage'

const styles = {
    userOverview: {
        width: "100%",
        maxWidth: 800,
    },
}

function UserManagement({ classes, layout: Layout }) {
    const user = JSON.parse(getItem("user"))
    return (<Layout>
        {user.privileged ? <UserOverview className={classes.userOverview} user={user} /> : null}
    </Layout>
    )
}

export default withStyles(styles)(UserManagement)