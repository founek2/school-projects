// n.log(n)+(n.n/2)
/**
 * Compute which points dominates others in two dimensions using brute force algorithm with small improvements
 * @param {Array} df - pre-sorted array of points
 * @param {Axis} Axis
 */
export default function (df, {x, y}) {
    const array = []

    for (const [idx, row] of df.entries()) {
        let valid = true
        for (let idx2 = idx + 1; idx2 < df.length; idx2++) {
            const row2 = df[idx2];
            if (y.asc) {
                if (row[y.label] <= row2[y.label]) {
                    valid = false
                    break
                }
            } else
                if (row[y.label] >= row2[y.label]) {
                    valid = false
                    break
                }
        }
        if (valid) array.push(row)
    }

    return array
}