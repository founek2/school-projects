import quickSort from './quickSort'

export function cmp({x: {label: x, asc: asc_x}, y: {label: y, asc: asc_y}}) {
    return (p1, p2) => {
        if (asc_x) {
            if (p1[x] < p2[x])
                return -1;
            if (p1[x] > p2[x])
                return 1;
        } else {
            if (p1[x] > p2[x])
                return -1;
            if (p1[x] < p2[x])
                return 1;
        }

        if (asc_y) {
            if (p1[y] < p2[y])
                return -1;
            if (p1[y] > p2[y])
                return 1;
        } else {
            if (p1[y] > p2[y])
                return -1;
            if (p1[y] < p2[y])
                return 1;
        }
        return 0;
    }
}

export default (df, x, y, asc_x, asc_y) => quickSort(df, cmp(x, y, asc_x, asc_y))