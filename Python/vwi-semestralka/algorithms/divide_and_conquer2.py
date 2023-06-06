
def divide_and_conquer_alg(df: list, x: str, y: str, asc_x: bool, asc_y: bool):
    out =  divide_and_conquer_alg_rec(df, x, y, asc_x, asc_y)
    # print("items count DAC: ", len(out))
    return out


def divide_and_conquer_alg_rec(df, x, y, asc_x, asc_y):
    if len(df) == 1:
        return df

    median_idx = len(df)//2
    left = df[0:median_idx]
    right = df[median_idx: len(df)]

    left_maxima = divide_and_conquer_alg_rec(left, x, y, asc_x, asc_y)
    right_maxima = divide_and_conquer_alg_rec(right, x, y, asc_x, asc_y)

    q = right_maxima[0]
    result_left = []
    if asc_y:
        for r in left_maxima:
            if r[y] > q[y]:
                result_left.append(r)
    else:
        for r in left_maxima:
            if r[y] < q[y]:
                result_left.append(r)

    return result_left + right_maxima