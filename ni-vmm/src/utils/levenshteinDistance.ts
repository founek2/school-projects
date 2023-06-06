const str1 = 'hitting';
const str2 = 'kitten';

/* bottom-up dynamic programming */
function levenshteinDistance(str1 = '', str2 = '') {
    // Create array M[str2.length][str1.length]
    const track = Array(str2.length + 1)
        .fill(undefined)
        .map(() => Array(str1.length + 1).fill(0));

    /* source prefixes can be transformed into empty string by
       dropping all characters */
    for (let i = 1; i <= str1.length; i += 1) {
        track[0][i] = i;
    }

    /* target prefixes can be reached from empty source prefix
       by inserting every character */
    for (let j = 1; j <= str2.length; j += 1) {
        track[j][0] = j;
    }

    for (let j = 1; j <= str2.length; j += 1) {
        for (let i = 1; i <= str1.length; i += 1) {
            const substitutionCost = str1[i - 1] === str2[j - 1] ? 0 : 1;
            track[j][i] = Math.min(
                track[j][i - 1] + 1, // deletion
                track[j - 1][i] + 1, // insertion
                track[j - 1][i - 1] + substitutionCost // substitution
            );
        }
    }
    return track[str2.length][str1.length];
}

// console.log(levenshteinDistance('book', 'back') === 2);
// console.log(levenshteinDistance('Algorithms', 'Applications') === 9);
// console.log(levenshteinDistance('subquadratic', 'It') === 11);

export function levenshteinDistanceBetter(str1 = '', str2 = '') {
    // Create array M[str2.length][str1.length]
    const n = Math.max(str1.length, str2.length);
    let v0 = Array(n + 1).fill(0);
    let v1 = Array(n + 1).fill(0);

    /* source prefixes can be transformed into empty string by
    dropping all characters */
    for (let i = 1; i < v0.length; i += 1) {
        v0[i] = i;
    }

    for (let j = 1; j <= str2.length; j += 1) {
        v1[0] = j;
        for (let i = 1; i <= str1.length; i += 1) {
            const substitutionCost = str1[i - 1] === str2[j - 1] ? 0 : 1;
            v1[i] = Math.min(
                v1[i - 1] + 1, // deletion
                v0[i] + 1, // insertion
                v0[i - 1] + substitutionCost // substitution
            );
        }

        const tmp = v0;
        v0 = v1;
        v1 = tmp;
    }
    return v0[str1.length];
}

// console.log(levenshteinDistance(str1, str2));
// console.log(levenshteinDistanceBetter('book', 'back') === 2);
// console.log(levenshteinDistanceBetter('Algorithms', 'Applications') === 9);
// console.log(levenshteinDistanceBetter('subquadratic', 'it') === 11);
// console.log(levenshteinDistanceBetter('aaaa', 'bbccc') === 5);
