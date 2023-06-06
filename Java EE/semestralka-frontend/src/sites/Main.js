import React, { Component } from 'react'
import Typography from '@material-ui/core/Typography'
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
import { append, curry, when, propEq, assoc, map, find, reverse, filter, o, not, sortWith, descend, ascend, prop } from 'ramda'
import { delItem, getItem } from '../utils/storage'
import UserOverview from '../components/UserOverview'
import Menu from '../components/Menu'

const alter = curry((checked, key, id, items) => map(when(propEq('id', id), assoc(key, checked)), items))

const sortTodo = sortWith([ascend(prop('done')), descend(prop('id'))])

const styles = theme => ({
     root: {
          display: 'flex',
          alignItems: 'center',
		justifyContent: 'center',
		flexDirection: "column"
     },
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
	userOverview: {
		width: "100%",
		maxWidth: 800,
	},
	input: {
		paddingTop: 30
	}
})

class Main extends Component {
     constructor(props) {
          super(props)
          this.state = {
               user: JSON.parse(getItem('user')),
               input: '',
               todos: []
          }
          if (!getItem('jwt')) props.history.push('/')
     }

     componentDidMount() {
          todoApi
               .fetchAll()
               .then(array => this.setState({ todos: array }))
               .catch((e) => {
				console.log('Cant get your todos from server', e)
				this.logOut()
			})
     }
     addTodo = todo => this.setState(state => ({ todos: append(todo, state.todos) }))

     checkItem = id => e => {
          const val = e.target.checked
          const todo = find(propEq('id', id), this.state.todos)
          console.log(val, 'todo ', id, todo, this.state.todos)
          const newTodo = assoc('done', val, todo)
          todoApi.update(newTodo).then(res => {
			if (res.status >= 400)
				alert("Nastala chyba, zkuste to prosím později.")
               else this.setState({ todos: alter(val, 'done', id, this.state.todos) })
          })
     }

     deleteItem = id => () => {
          const todo = find(propEq('id', id), this.state.todos)

          todoApi.del(todo).then(() => {
               this.setState({ todos: filter(o(not, propEq('id', id)), this.state.todos) })
          })
     }

     createTodo = () => {
          todoApi
               .create({ text: this.state.input })
               .then(todo => {
                    todo.dateTime = new Date().getTime()
                    this.addTodo(todo)
                    this.setState({ input: '' })
               })
               .catch(e => {
                    if (e == 400) alert('Text must have lenght > 0')
               })
     }

     logOut = () => {
		delItem('jwt')
		delItem('user')
          this.props.history.push('/')
     }

     render() {
          const { input, todos, user } = this.state
          const { classes } = this.props
          return (
                    <div className={classes.root}>
                         <Typography className={classes.userName}>Logged as: {user.userName}</Typography>
                         {/* <IconButton onClick={this.logOut} className={classes.exitButton}>
                              <ExitToAppIcon fontSize="default" />
					</IconButton> */}
                         <Menu className={classes.mainMenu} onLogout={this.logOut} userName={user.userName} />
                         <Paper className={classes.paper}>
                              <div className={classes.head}>
                                   <Typography variant="h3">TODO app</Typography>
                                   <div className={classes.input}>
                                        <TextField
                                             value={input}
                                             onChange={e => this.setState({ input: e.target.value })}
                                             margin="normal"
                                             label="New task"
                                             className={classes.textField}
                                        />
                                        <Fab
                                             color="primary"
                                             aria-label="Add"
                                             className={classes.fab}
                                             size="small"
                                             className={classes.addButton}
                                             onClick={this.createTodo}
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
                                                  onChange={this.checkItem(id)}
                                                  {...other}
                                                  onDelete={this.deleteItem(id)}
                                             />
                                        ))}
                                   </FormControl>
                              </div>
                         </Paper>
					{user.privileged ? <UserOverview className={classes.userOverview} user={user} /> : null}
             
                    </div>
          )
     }
}

export default withStyles(styles)(Main)
