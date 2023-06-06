import React, { Component } from 'react'
import Typography from '@material-ui/core/Typography'
import Paper from '@material-ui/core/Paper'
import TextField from '@material-ui/core/TextField'
import FormControl from '@material-ui/core/FormControl'
import { withStyles } from '@material-ui/core/styles'
import Checkbox from '@material-ui/core/Checkbox'
import Divider from '@material-ui/core/Divider'
import AddIcon from '@material-ui/icons/Add'
import DeleteIcon from '@material-ui/icons/Delete'
import FormControlLabel from '@material-ui/core/FormControlLabel'
import IconButton from '@material-ui/core/IconButton';

const styles = theme => ({
     lineOver: {
          textDecoration: 'line-through'
     },
     item: {
          wordBreak: 'break-word'
     },
     root: {
          marginTop: 10,
          position: 'relative',
		paddingBottom: 20,
		paddingRight: 20
     },
     dateTime: {
          position: 'absolute',
          right: 0,
          bottom: 0,
          fontSize: 12
	},
	deleteButton: {
		position: "absolute",
		right: -15
	}
})

function toString(d) {
	const date = new Date(d);
     return date.getHours() + ':' + date.getMinutes() + ' ' + date.getDate() + '. ' + date.getMonth() + '.'
}

function TodoItem({ classes, text, done, onChange, dateTime, onDelete }) {
     return (
          <div>
               <div className={classes.root}>
                    <FormControlLabel
                         control={<Checkbox checked={done} onChange={onChange} />}
                         label={text}
                         classes={{ root: classes.item }}
                         className={done ? ' ' + classes.lineOver : ''}
                    />
                    <span className={classes.dateTime}>{toString(dateTime)}</span>
                    <IconButton onClick={onDelete} className={classes.deleteButton}>
                         <DeleteIcon fontSize="small" />
                    </IconButton>
               </div>
               <Divider />
          </div>
     )
}

export default withStyles(styles)(TodoItem)
