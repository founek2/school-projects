
def sweep_plane_alg(df: list, x: str, y: str, asc_x: bool, asc_y: bool):
    stack = []

    for row in df:
        if asc_y:
            while stack and stack[len(stack) - 1][y] <= row[y]:
                stack.pop()
        else:
            while stack and stack[len(stack) - 1][y] >= row[y]:
                stack.pop()

        stack.append(row)

    # print("items count SP: ", len(stack))
    # print("stack", stack)
    return stack