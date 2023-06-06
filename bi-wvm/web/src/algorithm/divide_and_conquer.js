// T(n) = 2T(n / 2) + O(n log n)
// Master Theorem ->  O(n log2 n)
/**
 * Compute which points dominates others in two dimensions using divice and conquer algorithm
 * @param {Array} df 
 * @param {Axis} axis
 */
export default function divide_and_conquer_alg(df, {x, y}){
    const out =  divide_and_conquer_alg_rec(df, x, y)
    // console.log("items count DAC: ", out.length)
    return out
}

function divide_and_conquer_alg_rec(df, x, y){
    if (df.length == 1)
        return df

    const median_idx = Math.floor(df.length / 2)
    const left = df.slice(0,median_idx)
    const right = df.slice(median_idx, df.length)

    const left_maxima = divide_and_conquer_alg_rec(left, x, y)
    const right_maxima = divide_and_conquer_alg_rec(right, x, y)

    const q = right_maxima[0]
    const result_left = []

    if (y.asc)
        for (const r of left_maxima){
            if (r[y.label] > q[y.label])
                result_left.push(r)
        }
    else
        for (const r of left_maxima)
            if (r[y.label] < q[y.label])
                result_left.push(r)

    return result_left.concat(right_maxima)
}