# n.log(n)+(n.n/2)
def brute_force_alg(df, x, y, asc_x=True, asc_y=True):
    df = df.sort_values(by=[x, y], ascending=[asc_x, asc_y])
    df =df.reset_index(drop=True)
    # reset index so I can use indexing later
    df = df[[x, y, "Id"]]

    array = []

    if asc_y:
        for idx, row in df.iterrows():
            valid = True
            for idx2, row2 in df.iloc[idx + 1:].iterrows():  # +1 works without exception
                if row[y] <= row2[y]:
                    valid = False
                    break
            if valid: array.append(row)
    else:
        for idx, row in df.iterrows():
            valid = True
            for idx2, row2 in df.iloc[idx + 1:].iterrows():  # +1 works without exception
                if row[y] >= row2[y]:
                    valid = False
                    break
            if valid: array.append(row)

    print("items count BF: ", len(array))
    return array
