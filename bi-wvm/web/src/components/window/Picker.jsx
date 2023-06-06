import React, { useState } from 'react'
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import Typography from '@material-ui/core/Typography';
import Button from '@material-ui/core/Button';
import Grid from '@material-ui/core/Grid';
import { makeStyles } from '@material-ui/core/styles';
import { update } from 'ramda'
import clsx from 'clsx'
import ArrowDropUpIcon from '@material-ui/icons/ArrowDropUp'
import Tooltip from '@material-ui/core/Tooltip';

const useStyles = makeStyles((theme) => ({
    root: {
        flexGrow: 1,
        maxWidth: 752,
    },
    demo: {
        backgroundColor: theme.palette.background.paper,
    },
    title: {
        margin: theme.spacing(4, 0, 2),
    },
    selected: {
        color: theme.palette.secondary.dark
    },
    listItem: {
        cursor: "pointer"
    },
    rotate: {
        transform: "rotate(180deg)"
    }
}));


function Picker({ dense = true, data, onSubmit, defaultAxis }) {
    const classes = useStyles();
    const [axis, setAxis] = useState([defaultAxis.x.label, defaultAxis.y.label])
    const [asc, setAsc] = useState([defaultAxis.x.asc, defaultAxis.y.asc])


    return (<div>

        <Typography variant="h6" className={classes.title}>
            Selection
          </Typography>
        <div className={classes.demo}>
            <List dense={dense} className={classes.list}>
                {data.map(label => {
                    const idx = axis.indexOf(label);
                    return (
                        <ListItem key={label} className={clsx(idx > - 1 && classes.selected, classes.listItem)} onClick={() => {
                            if (idx > - 1)
                                setAsc(update(idx, !asc[idx], asc))
                            else
                                setAxis([axis[1], label])
                        }}>
                            <Tooltip title={idx == 0 ? "Osa X" : idx == 1 ? "Osa Y" : ""}><ListItemText
                                primary={label}
                            /></Tooltip>
                            {idx > - 1 && <Tooltip title={asc[idx] ? "Důležitá vyšší hodnota" : "Důležitá nižší hodnota"}>
                                <ArrowDropUpIcon className={clsx(asc[idx] && classes.rotate)} />
                                </Tooltip>}
                        </ListItem>
                    )
                })}
            </List>
        </div>
        <Button onClick={(e) => {

            onSubmit({
                x: {
                    label: axis[0],
                    asc: asc[0]
                },
                y: {
                    label: axis[1],
                    asc: asc[1]
                }
            })
        }}>Submit</Button>
    </div>)
}


export default Picker