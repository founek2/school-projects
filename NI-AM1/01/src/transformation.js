function tail(arr) {
    return arr[arr.length - 1];
}
function head(arr) {
    return arr[0];
}

const SPACE = ' ';

function transformToJson(body) {
    // Retrieve important lines between '==='
    [inside_lines] = body.split('\n').reduce(
        ([data, isInside], line) => {
            if (line === '===') {
                if (isInside) return [data, false];
                else return [data, true];
            } else {
                if (isInside) return [[...data, line], isInside];
                else return [data, isInside];
            }
        },
        [[], false]
    );

    // convert lines in format '{some key}: {some value}' to pairs key/value, remove double quotes around values
    const pairs = inside_lines
        .map((line) => {
            const [key, value] = line.split(': ');
            return [
                tail(key.split(SPACE)).toLocaleLowerCase(),
                value.replace(/\"/g, ''),
            ];
        })
        .map(([key, value]) =>
            key === 'person'
                ? [
                      key,
                      {
                          name: head(value.split(SPACE)),
                          surname: tail(value.split(SPACE)),
                      },
                  ]
                : [key, value]
        );

    // Convert pairs into Object and set as response
    return Object.fromEntries(pairs);
}

module.exports = { transformToJson };
