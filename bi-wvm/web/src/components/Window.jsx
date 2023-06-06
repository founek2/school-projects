import React, { useState } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Table from './window/Table'
import Chart from './window/Chart'
import Picker from './window/Picker'
import Grid from '@material-ui/core/Grid';
import csvToArray from '../utils/csvToArray'
import csv from '../partial2000.csv.js'

import brute_force from '../algorithm/brute_force'
import sweep_plane from '../algorithm/sweep_plane'
import divide_and_conquer from '../algorithm/divide_and_conquer'
import make_edges from '../utils/make_edges'
var TimSort = require('timsort');
import { cmp } from '../utils/myQuickSort'

const useStyles = makeStyles((theme) => ({
    root: {
        flexGrow: 1,
    },
    paper: {
        padding: theme.spacing(2),
        textAlign: 'center',
        color: theme.palette.text.secondary,
    },
}));

const defaultAxis = {
    x: {
        label: "Price",
        asc: true
    },
    y: {
        label: "Mileage",
        asc: true
    }
}

//const origData = csvToArray(csv).slice(0, 1500)
const origData = csvToArray(csv)
const initData = origData.slice()
TimSort.sort(initData, cmp(defaultAxis))
console.log({ initData })

const COLUMNS = [{
    label: "Id", dataKey: "Id", width: 60, type: "number"
},
{
    label: "Price", dataKey: "Price", width: 120, type: "number"
},
{
    label: "Mileage", dataKey: "Mileage", width: 120, type: "number"
},
{
    label: "Year", dataKey: "Year", width: 120, type: "number"
},
{
    label: "City", dataKey: "City", width: 140
},
{
    label: "State", dataKey: "State", width: 80
},
{
    label: "Vin", dataKey: "Vin", width: 250
},
{
    label: "Make", dataKey: "Make", width: 120
},
{
    label: "Model", dataKey: "Model", width: 120
}]

function Window() {
    const [data, setData] = useState(initData)
    const [axis, setAxis] = useState(defaultAxis)

    function handleSubmit(axis) {
        console.log("submit", axis)
        const sorted = data.slice()
        TimSort.sort(sorted, cmp(axis))
        setData(sorted)
        setAxis(axis)
    }
    const classes = useStyles();

    const { x, y } = axis
    const chartData = [
        [x.label, y.label, "Skyline sweep", { role: "tooltip", type: "string", p: { html: true } }]
    ];

    const skylines = [[], [], []]
    let start = new Date()
    skylines[0] = make_edges(divide_and_conquer(data, axis), axis)
    console.log(`divide_and_conquer > ${new Date() - start} milliseconds`)

    start = new Date()
    skylines[1] = make_edges(brute_force(data, axis), axis)
    console.log(`brute_force > ${new Date() - start} milliseconds`)

    start = new Date()
    skylines[2] = make_edges(sweep_plane(data, axis), axis)
    console.log(`sweep_plane > ${new Date() - start} milliseconds`)

    // const skyline =[]
    const skyline = skylines[2];
    const KEYS = Object.keys(data[0])

    data.forEach(row => {
        let tooltip = `<div class="chart-tooltip">`;
        KEYS.forEach(k => {
            tooltip += `<label>${k}: <\label><span>${row[k]}<\span><br>`
        })
        tooltip += `<\div>`;
        const linePoints = skyline.filter(item => item[x.label] === row[x.label])

        if (linePoints.length === 0)    // no points on skyline with x == current row.x
            chartData.push(
                [row[x.label], row[y.label], null, tooltip]
            )
        else {
            linePoints.forEach(point => {  // add point on skyline and point itself
                chartData.push(
                    [row[x.label], point.edge ? null : row[y.label], point[y.label], point.edge ? "edge point" : tooltip]
                )
            })
        }
    })

    return (
        <div className={classes.root}>
            <Grid container>
                <Grid item xs={11} md={11}>
                    <Chart
                        axis={axis}
                        data={chartData}
                        title="Skyline"
                    />
                </Grid>
                <Grid item>
                    <Picker xs={1} md={1} defaultAxis={defaultAxis} onSubmit={handleSubmit} data={COLUMNS.filter(col => col.type === "number").map(col => col.label)} />
                </Grid>

                <Grid item xs={12} md={12}>
                    <Table rows={origData} columns={COLUMNS} />
                </Grid>
            </Grid>
        </div>)
}

export default Window;