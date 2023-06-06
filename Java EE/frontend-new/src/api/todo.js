import { getItem } from '../utils/storage'

const PREFIX = '/api/todo'

export function update(TodoItem) {
    const { id } = TodoItem
    delete TodoItem.id
    return fetch(PREFIX + "/" + id, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json', 'Auth-Token': getItem('jwt') },
        body: JSON.stringify(TodoItem)
    })
}

export function create(TodoItem, boardId) {
    return fetch(PREFIX + "/" + boardId, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', Accept: 'application/json', 'Auth-Token': getItem('jwt') },
        body: JSON.stringify(TodoItem)
    })
        .then(res => {
            if (res.status >= 300) throw res.status
            else return res
        })
        .then(res => res.json())
}

export function del(todoId) {
    return fetch(PREFIX + "/" + todoId, {
        method: 'DELETE',
        headers: { 'Auth-Token': getItem('jwt') }
    })
}

// export function fetchAll() {
//      return fetch(PREFIX, {
//           method: 'GET',
//           headers: { Accept: 'application/json', 'Auth-Token': getItem('jwt') }
//      }).then(res => res.json())
// }
