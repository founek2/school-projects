import React, { Component, Fragment } from 'react'
import Typography from '@material-ui/core/Typography'
import Divider from '@material-ui/core/Divider'
import Paper from '@material-ui/core/Paper'
import TextField from '@material-ui/core/TextField'
import FormControl from '@material-ui/core/FormControl'
import { withStyles } from '@material-ui/core/styles'
import Fab from '@material-ui/core/Fab'
import AddIcon from '@material-ui/icons/Add'
import ExitToAppIcon from '@material-ui/icons/ExitToApp'
import IconButton from '@material-ui/core/IconButton'
import TodoItem from '../components/TodoItem'
import * as todoApi from '../api/todo'
import * as boardApi from '../api/board'
import { append, curry, when, propEq, assoc, map, find, reverse, filter, o, update, sortWith, descend, ascend, prop, insert } from 'ramda'
import { delItem, getItem } from '../utils/storage'
import UserOverview from '../components/UserOverview'
import Board from '../components/Board'
import { withRouter } from "react-router-dom"


const styles = theme => ({
    paper: {
        maxWidth: 400,
        width: "100%",
        paddingTop: 30,
        textAlign: 'center',
    },
    inputBoard: {
        paddingTop: 30,
        display: "flex",
        flexDirection: "column",
        alignItems: "center"
    }
})

class Main extends Component {
    constructor(props) {
        super(props)
        this.state = {
            user: JSON.parse(getItem('user')),
            input: '',
            boards: []
        }
        if (!getItem('jwt')) props.history.push('/')
    }

    componentDidMount() {
        boardApi
            .fetchAll()
            .then(array => this.setState({ boards: array }))
            .catch((error) => {
                if (error === 403) this.props.history.push("/")
                console.log('Cant get your todos from server', error)
            })
    }
    // addTodo = todo => this.setState(state => ({ todos: append(todo, state.todos) }))
    createBoard = () => {
        const name = this.state.input
        boardApi.createBoard(name).then((board) =>
            this.setState({
                boards: append(board,
                    this.state.boards)
            })
        )
    }

    setTodos = (boardId) => (newTodos) => {
        const id = this.state.boards.findIndex(({ id }) => id === boardId)
        const board = assoc("todoItems", newTodos, this.state.boards[id])
        this.setState({
            boards: update(id, board, this.state.boards)
        })

        console.log("boards", this.state.boards, "todos", newTodos, "boardIndex", id, update(id, board, this.state.boards))
    }

    render() {
        const { input, boards, user } = this.state
        const { classes, layout: Layout } = this.props
        console.log("borads", boards)
        return (
            <Layout>
                {boards.map(({ id, name, todoItems }) => <Fragment key={id}>
                    <Board todos={todoItems} name={name} id={id} setTodos={this.setTodos(id)} />
                    <Divider />
                </Fragment>)}
                <Paper className={classes.paper}>
                    <Typography variant="h4">Vytvořit board</Typography>

                    <div className={classes.inputBoard}>
                        <TextField
                            value={input}
                            onChange={e => this.setState({ input: e.target.value })}
                            margin="normal"
                            label="Název boardu"
                            className={classes.textField}
                        />
                        <Fab
                            color="primary"
                            aria-label="Add"
                            className={classes.fab}
                            size="large"
                            className={classes.addButton}
                            onClick={this.createBoard}
                        >
                            <AddIcon />
                        </Fab>
                    </div>
                </Paper>
            </Layout>
        )
    }
}

export default withRouter(withStyles(styles)(Main))
