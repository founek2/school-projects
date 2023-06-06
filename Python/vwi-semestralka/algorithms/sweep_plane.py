import time

def sweep_plane_alg(df, x, y, asc_x = True, asc_y = True):

    df = df.sort_values(by=[x, y], ascending=[asc_x, asc_y])

    df = df[[x, y, "Id"]]   # it gives a little speed up

    stack = []
    for idx, row in df.iterrows():
        if asc_y:
            while stack and stack[len(stack) - 1][y] <= row[y]:
                stack.pop()
        else:
            while stack and stack[len(stack) - 1][y] >= row[y]:
                stack.pop()

        stack.append(row)

    print("items count SP: ", len(stack))
    # print("stack", stack)
    return stack

def make_edges(stack, x, y, asc_x, asc_y):
    ret = []
    for i in range(len(stack ) - 1):
        a = stack[i]
        b = stack[i + 1]

        ret.append(a)
        ret.append(get_edge_point(a, b, x, y,asc_x, asc_y))

    ret.append(stack[-1])
    return ret

def get_edge_point(a, b, x, y,asc_x, asc_y):
    d = {}
    if asc_x and asc_y:
        d[x]= a[x]
        d[y]= b[y]
    elif asc_x and not asc_y:
        d[x] = b[x]
        d[y] = a[y]
    elif not asc_x and asc_y:
        d[x] = b[x]
        d[y] = a[y]
    else:   # not asc_x and not asc_y
        d[x] = a[x]
        d[y] = b[y]

    return d
