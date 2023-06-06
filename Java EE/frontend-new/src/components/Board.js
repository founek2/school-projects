import React, { useState } from 'react'
import Typography from '@material-ui/core/Typography'
import Paper from '@material-ui/core/Paper'
import TextField from '@material-ui/core/TextField'
import FormControl from '@material-ui/core/FormControl'
import { withStyles } from '@material-ui/core/styles'
import Fab from '@material-ui/core/Fab'
import AddIcon from '@material-ui/icons/Add'
import ExitToAppIcon from '@material-ui/icons/ExitToApp'
import IconButton from '@material-ui/core/IconButton'
import TodoItem from './TodoItem'
import * as todoApi from '../api/todo'
import * as boardApi from '../api/board'
import { append, curry, when, propEq, assoc, map, find, reverse, filter, o, not, sortWith, descend, ascend, prop } from 'ramda'

const alter = curry((checked, key, id, items) => map(when(propEq('id', id), assoc(key, checked)), items))

const sortTodo = sortWith([ascend(prop('done')), descend(prop('id'))])

const styles = theme => ({
    paper: {
        maxWidth: 700,
        width: "100%"
    },
    textField: {
        marginLeft: theme.spacing(1),
        marginRight: theme.spacing(1),
        maxWidth: 350,
        width: "70%"
    },
    formControl: {
        paddingLeft: 45,
        paddingRight: 45,
        width: '100%'
    },
    addButton: {
        top: 27
    },
    head: {
        textAlign: 'center',
        paddingTop: 50
    },
    items: {
        paddingBottom: 70,
        paddingTop: 20,
        overflowY: 'scroll',
        maxHeight: 500
    },
    exitButton: {
        position: 'absolute',
        right: 12,
        top: 10
    },

    input: {
        paddingTop: 30
    },
})

function Board({ todos, name, id, classes, setTodos }) {
    const [input, setInput] = useState("")

    function checkItem(id) {
        return e => {
            const val = e.target.checked
            const todo = find(propEq('id', id), todos)

            const newTodo = assoc('done', val, todo)
            todoApi.update(newTodo).then(res => {
                if (res.status >= 400)
                    alert("Nastala chyba, zkuste to prosím později.")
                else setTodos(alter(val, 'done', id, todos))
            })
        }
    }

    function deleteItem(id) {
        return () => {
            const todo = find(propEq('id', id), todos)

            todoApi.del(todo.id).then(() => {
                setTodos(filter(o(not, propEq('id', id)), todos))
            })
        }
    }

    function createTodo(boardId) {
        return () => {
            todoApi
                .create({ text: input }, boardId)
                .then(todo => {
                    todo.dateTime = new Date().getTime()
                    setTodos(append(todo, todos))
                    this.setState({ input: '' })
                })
                .catch(e => {
                    if (e === 400) alert('Text must have lenght > 0')
                })
        }
    }

    return (<Paper className={classes.paper}>
        <div className={classes.head}>
            <Typography variant="h3">{name}</Typography>
            <div className={classes.input}>
                <TextField
                    value={input}
                    onChange={e => setInput(e.target.value)}
                    margin="normal"
                    label="New task"
                    className={classes.textField}
                />
                <Fab
                    color="primary"
                    aria-label="Add"
                    // className={classes.fab}
                    size="small"
                    className={classes.addButton}
                    onClick={createTodo(id)}
                >
                    <AddIcon />
                </Fab>
            </div>
        </div>
        <div className={classes.items}>
            <FormControl component="fieldset" className={classes.formControl}>
                {sortTodo(todos).map(({ id, ...other }) => (
                    <TodoItem
                        key={id}
                        onChange={checkItem(id)}
                        {...other}
                        onDelete={deleteItem(id)}
                    />
                ))}
            </FormControl>
        </div>
    </Paper>)
}

export default withStyles(styles)(Board)