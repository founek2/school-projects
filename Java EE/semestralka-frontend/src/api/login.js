const PREFIX = '/api/login'

export function signIn(User) {
     return fetch(PREFIX, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
          body: JSON.stringify(User)
     })
          .then(res => {
               if (res.status == 403) throw 403
               else return res
          })
          .then(res => res.json())
          .then(json => json)
}
