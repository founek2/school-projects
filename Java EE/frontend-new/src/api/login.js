import { getItem } from '../utils/storage'

const PREFIX = '/api'

export function signIn(User) {
    return fetch(PREFIX + "/sign_in", {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
        body: JSON.stringify(User)
    })
        .then(res => {
            if (res.status === 403) throw 403
            else return res
        })
        .then(res => res.json())
        .then(json => json)
}

export function signOut(User) {
    return fetch(PREFIX + "/sign_out", {
        method: 'POST',
        headers: { 'Auth-Token': getItem('jwt') },

    })
}
