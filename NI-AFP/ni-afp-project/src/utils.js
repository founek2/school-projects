export function onEnter(cb) {
    return (e) => {
        if (e.key === 'Enter') {
            cb(e);
        }
    };
}
