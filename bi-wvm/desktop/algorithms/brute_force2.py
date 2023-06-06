
# n.log(n)+(n.n/2)
def brute_force_alg(df: list, x: str, y: str, asc_x: bool, asc_y: bool):
    array = []

    if asc_y:
        for idx in range(0, len(df)):
            row = df[idx]
            valid = True
            for row2 in df[idx + 1:]:  # +1 works without exception
                if row[y] <= row2[y]:
                    valid = False
                    break
            if valid: array.append(row)
    else:
        for idx in range(0, len(df)):
            row = df[idx]
            valid = True
            for row2 in df[idx + 1:]:  # +1 works without exception
                if row[y] >= row2[y]:
                    valid = False
                    break
            if valid: array.append(row)

    # print("items count BF: ", len(array))
    return array
