/**
 * 
 * @param {*} stack 
 * @param {Axis} axis
 */
export default function (stack, axis) {
    const ret = []
    for (let i = 0; i < stack.length - 1; i++) {
        const a = stack[i]
        const b = stack[i + 1]

        ret.push(a)
        ret.push(get_edge_point(a, b, axis))
    }
    ret.push(stack[stack.length - 1])
    return ret
}

function get_edge_point(a, b, { x: { asc: asc_x, label: x }, y: { asc: asc_y, label: y } }) {
    if (asc_x && asc_y)
        return { [x]: a[x], [y]: b[y], edge: true }
    if (asc_x && !asc_y)
        return { [x]: b[x], [y]: a[y], edge: true }
    if (!asc_x && asc_y)
        return { [x]: b[x], [y]: a[y], edge: true }

    //   # not asc_x and not asc_y
    return { [x]: a[x], [y]: b[y], edge: true }
}