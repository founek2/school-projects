const store = localStorage

export function getItem(key: string) {
     return store.getItem(key)
}

export function storeItem(key: string, val: string) {
     store.setItem(key, val)
}

export function delItem(key: string) {
	store.removeItem(key)
}
