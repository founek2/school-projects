/**
 * Compute which points dominates others in two dimensions using fast sweep plane algorithm
 * @param {Array} df 
 * @param {Axis} axis
 */
export default function (df, {x, y}){
    const stack = []

    df.forEach((row) => {
        if (y.asc)
            while (stack && stack.length > 0 && stack[stack.length - 1][y.label] <= row[y.label])
                stack.pop()
        else
            while (stack && stack.length > 0 && stack[stack.length - 1][y.label] >= row[y.label])
                stack.pop()

        stack.push(row)
    })
        
    // console.log("items count SP: ", stack.length)
    return stack
}