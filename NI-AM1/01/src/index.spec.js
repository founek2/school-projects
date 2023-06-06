const { transformToJson } = require('./transformation');
const assert = require('assert');

const data_1 = `
Dear Sir or Madam,

please find the details about my booking below:

===
Tour id: "1"
Location: "Bohemian Switzerland"
Person: "Jan Novak"
===

Regards,
Jan Novak
`;

assert.deepEqual(transformToJson(data_1), {
    id: '1',
    location: 'Bohemian Switzerland',
    person: {
        name: 'Jan',
        surname: 'Novak',
    },
});

const data_2 = `
Dear Sir or Madam,

please find the details about my booking below:

===
Tour id: "11"
Location: "Czech Republic"
Person: "Filip Stránský"
===

Regards,
Filip Stránský
`;

assert.deepEqual(transformToJson(data_2), {
    id: '11',
    location: 'Czech Republic',
    person: {
        name: 'Filip',
        surname: 'Stránský',
    },
});
