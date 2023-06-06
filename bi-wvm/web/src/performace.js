import brute_force from './algorithm/brute_force'
import sweep_plane from './algorithm/sweep_plane'
import divide_and_conquer from './algorithm/divide_and_conquer'
import csvToArray from './utils/csvToArray'
var TimSort = require('timsort');
import { cmp } from './utils/myQuickSort'
import {equals} from 'ramda'
import assert from 'assert'

var readlines = require('n-readlines');
var liner = new readlines("/Users/martas/Nextcloud/projects/All/vwm-semestralka-fe/partial1m.csv");
const headers = liner.next().toString('ascii').split(",")

const data = []
let next

console.log("Loading data...")
while (next = liner.next()) {
    const row = next.toString('ascii').split(',')
    const obj = {}
    for (let j = 0; j < row.length; j++) {
        if (isNaN(row[j]))
            obj[headers[j]] = row[j];
        else
            obj[headers[j]] = Number(row[j]);
        // obj[header[i]] = row[i]
    }
    data.push(obj)
}

// console.log("itemsCount", data.length)
// const data = csvToArray(csv)
const x = { label: "Year", asc: true }
const y = { label: "Price", asc: true }
const axis = {x, y}

console.log("Sorting...")

TimSort.sort(data, cmp(axis))

console.log("")
console.log("Starting calculations...")

const num = 10
for (let cnt of [50, 1000, 10000, 100000, 1000000]) {
// for (let cnt of [50]) {
    const times = [0, 0, 0];
    const skyline = [[], [], []]
    const currData = data.slice(0, cnt)
    for (let i = 0; i < num; i++) {

        let start = Date.now()
        skyline[0] = sweep_plane(currData, axis)
        times[0] += Date.now() - start

        start = Date.now()
        skyline[1] = brute_force(currData, axis)
        times[1] += Date.now() - start

        start = Date.now()
        skyline[2] = divide_and_conquer(currData, axis)
        times[2] += Date.now() - start
    }
    // console.log("delky", skyline[0].length, skyline[1].length, skyline[2].length)
    assert.deepStrictEqual(skyline[0], skyline[1])
    assert.deepStrictEqual(skyline[1], skyline[2])

    console.log("Items COUNT>", cnt,)
    console.log("Sweep plane alg - ", times[0] / num / 1000, "sec")
    console.log("Brute force alg - ", times[1] / num / 1000, "sec")
    console.log("Divide and conc alg - ", times[2] / num / 1000, "sec")
    console.log("")
}