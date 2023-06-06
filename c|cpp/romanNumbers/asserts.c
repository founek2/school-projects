assert(arabify2("MCDXLI", 6) == 1441);
assert(arabify2("XII", 3) == 12);
assert(arabify2("VI", 2) == 6);
assert(arabify2("VIII", 4) == 8);
assert(arabify2("IV", 2) == 4);

assert(arabify2("I", 1) == 1);
assert(arabify2("II", 2) == 2);
assert(arabify2("III", 3) == 3);
assert(arabify2("IV", 2) == 4);
assert(arabify2("V", 1) == 5);
assert(arabify2("VI", 2) == 6);
assert(arabify2("VIII", 4) == 8);
assert(arabify2("IX", 2) == 9);
assert(arabify2("XIV", 3) == 14);
assert(arabify2("LXXXVIII", 8) == 88);
assert(arabify2("XC", 2) == 90);
assert(arabify2("XCIX", 4) == 99);
assert(arabify2("MXCIX", 5) == 1099);

assert(arabify2("DXLIV", 5) == 544);
assert(arabify2("DXL", 3) == 540);

//end
assert(isRomanValid2("MCMXLIV", 7));

assert(isRomanValid2("MDCLXVIV", 8) == 0);
assert(isRomanValid2("IIIV", 4) == 0);
assert(isRomanValid2("IIXI", 4) == 0);
assert(isRomanValid2("CCCXXXIII", 9));
assert(isRomanValid2("IIIVI", 5) == 0);

assert(isRomanValid2("CIX", 3));
assert(isRomanValid2("CCCC", 4) == 0);

assert(isRomanValid2("IVIII", 5) == 0);
assert(isRomanValid2("VL", 2) == 0);
assert(isRomanValid2("IV", 2));

assert(isRomanValid2("MMD", 3));
assert(isRomanValid2("MDM", 3) == 0);

assert(isRomanValid2("DD", 2) == 0);

assert(isRomanValid2("DLV", 3));

assert(isRomanValid2("IL", 2) == 0);
assert(isRomanValid2("IC", 2) == 0);
assert(isRomanValid2("ID", 2) == 0);
assert(isRomanValid2("IM", 2) == 0);
assert(isRomanValid2("XD", 2) == 0);
assert(isRomanValid2("CM", 2));
