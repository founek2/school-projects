import { getItem } from '../utils/storage'

const PREFIX = '/api/users'

export function fetchAll(TodoItem) {
     return fetch(PREFIX, {
          method: 'GET',
          headers: { 'Accept': 'application/json', 'Auth-Token': getItem('jwt') },
          body: JSON.stringify(TodoItem)
     }).then(res => res.json())
}

export function del(arryOfIds) {
     return fetch(PREFIX, {
          method: 'DELETE',
          headers: { 'Content-Type': 'application/json', 'Auth-Token': getItem('jwt') },
          body: JSON.stringify(arryOfIds)
     })
}

export function update(user) {
	const {id} = user;
	delete user.id;
     return fetch(PREFIX + "/" + id, {
          method: 'PATCH',
          headers: { 'Content-Type': 'application/json', 'Auth-Token': getItem('jwt') },
          body: JSON.stringify(user)
     })
}