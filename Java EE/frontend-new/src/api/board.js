import { getItem } from '../utils/storage'

const PREFIX = '/api/board'

export function fetchAll(User) {
    return fetch(PREFIX, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json', Accept: 'application/json', 'Auth-Token': getItem('jwt') },
        body: JSON.stringify(User)
    })
        .then(res => {
            if (res.status == 403) throw 403
            else return res
        })
        .then(res => res.json())
        .then(json => json)
}

export function createBoard(name) {
    return fetch(PREFIX, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', Accept: 'application/json', 'Auth-Token': getItem('jwt') },
        body: JSON.stringify({ name })
    }).then(res => res.json())
}