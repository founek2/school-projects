import pandas as pd

def divide_and_conquer_alg(df, x, y, asc_x, asc_y):
    df = df.sort_values(by=[x, y], ascending=[asc_x, asc_y])

    out =  divide_and_conquer_alg_rec(df[[x, y, "Id"]], x, y, asc_x, asc_y)
    print("items count DAC: ", len(out))
    return out


def divide_and_conquer_alg_rec(df, x, y, asc_x, asc_y):
    if len(df) == 1:
        return [df.iloc[0]]

    median_idx = len(df)//2
    left = df.iloc[0:median_idx]
    right = df.iloc[median_idx: len(df)]

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

if __name__ == "__main__":
    df1 = df = pd.DataFrame({"x": [2, 3],
                             "y": [1, 3]})
    divide_and_conquer_alg(df1, "x", "y", True, True)

"""
[(2,1), (3,3), (3, 5), (4, 4), (5, 5), (7, 5), (10, 12), (13, 8), (16, 10), (18, 9)]
# 1 split
    left = [(2,1), (3,3), (3, 5), (4, 4), (5, 5)]
    right = [(7, 5), (10, 12), (13, 8), (16, 10), (18, 9)]
    
    left_max = divide(left)
    right_max = divide(right)
    
    
"""

"""
[(2,1), (3,3), (3, 5), (4, 4), (5, 5)]
# 2 split
    left = [(2,1), (3,3), (3, 5)]
    right = [ (4, 4), (5, 5)]
    
    left_max = divide((2, 1))
    right_max = divide((3,3))
    
    
"""

"""
[(2,1), (3,3), (3, 5)]
# 3 split
    left = [(2,1), (3,3)]
    right = [ (3,5)) ]

    left_max = divide(left)
    right_max = divide(right)


"""

"""
[(2,1), (3,3) ]
# 3 split
    left = [(2,1)]
    right = [ (3,3) ]

    r = [(2,1)] = divide(left)
    l = [(3,3)] = divide(right)

    
"""