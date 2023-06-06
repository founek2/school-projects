const PREFIX = '/api/registration'

export function getNewId() {
     return fetch(PREFIX, { method: 'PUT' })
          .then(res => res.json())
          .then(json => json.id)
}

export function createUser(User) {
     return fetch(PREFIX, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
          body: JSON.stringify(User)
     })
}

export function fetchAll(User) {
     return fetch(PREFIX, {
          method: 'GET',
          headers: { Accept: 'application/json' }
     }).then(res => res.json())
}