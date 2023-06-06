//
// Created by Martin Skalický on 08/04/2019.
//

#ifndef __PROGTEST__

#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <cmath>
#include <iostream>
#include <iomanip>
#include <memory>
#include <string>

using namespace std;

class InvalidIndexException {
};

#endif /* __PROGTEST__ */

struct StrData {
    StrData() : numOfPonters(0) {};
    int numOfPonters;
    char *data;
    unsigned long len;

    char *get_c() {
        return data;
    }

    void set_c(char *str) {
        data = str;
    }

    ~StrData() {
        delete[] data;
    }
};

struct String {
    String() : offset(0), data(nullptr), len(0) {};

    String(const char *ptr) : offset(0), data(nullptr), position(0) {
        unsigned long strLen = strlen(ptr);
        data = new StrData;
        char *tmp = new char[strLen + 1];
        strncpy(tmp, ptr, strLen + 1);
        data->set_c(tmp);
        data->len = strLen;
        data->numOfPonters += 1;
        len = strLen;
    }

    String(const String &str) {
        data = str.data;
        offset = str.offset;
        len = str.len;
        if (str.data != nullptr)
            data->numOfPonters += 1;
        position = str.position;
    }

    String &operator=(const String &str) {
        if (this == &str) return *this;

        if (data != nullptr) {
            data->numOfPonters -= 1;
            if (data->numOfPonters == 0)
                delete data;
        }

        data = str.data;
        if (data != nullptr)
            data->numOfPonters += 1;
        offset = str.offset;
        len = str.len;
        position = str.position;
        return *this;
    }

    ~String() {
        if (data != nullptr) {
            if (data->numOfPonters <= 1) {
                delete data;
                data = nullptr;
            } else {
                data->numOfPonters -= 1;
            }
        }
    }

    size_t offset;
    StrData *data;
    size_t position;
    size_t len;
};

bool cmpFn(const String &s, size_t i) {
    return s.position + s.len < i;
}

void cp(String &dest, String &src) {
    dest.data = src.data;
    dest.offset = src.offset;
    dest.len = src.len;
    if (src.data != nullptr)
        dest.data->numOfPonters += 1;
    dest.position = src.position;

}

class Array {
public:

    void push(String &str) {
        if (len == arraySize) {
            reallocate();
        }
        if (len == 0) {
            str.position = 0;
        } else
            str.position = m_array[len - 1].position + m_array[len - 1].len;

        m_array[len++] = str;
    }

    void push(const Array &array1) {

        arraySize = len + array1.len + 10;
        auto *newArray = new String[arraySize];

        size_t i = 0;
        for (i = 0; i < len; ++i) {
            newArray[i] = m_array[i];
        }
        size_t position = 0;
        if (len > 0)
            position = m_array[i - 1].position + m_array[i - 1].len;
        for (size_t j = 0; j < array1.len; ++j) {
            String newStr = String(array1[j]);
            newStr.position = position;
            newArray[i + j] = newStr;

            position += newStr.len;
        }

        delete[] m_array;
        len = len + array1.len;
        m_array = newArray;
    }

    Array() {
        len = 0;
        arraySize = 10;
        m_array = new String[arraySize];
    }

    Array(const Array &arr) {
        len = arr.len;
        arraySize = arr.arraySize;
        m_array = new String[arraySize];
        copy(arr.m_array, arr.m_array + arr.arraySize, m_array);
    }

    ~Array() {
        delete[] m_array;
    }

    Array &operator=(const Array &arr) {
        if (this == &arr) return *this;

        len = arr.len;
        arraySize = arr.arraySize;
        delete[] m_array;
        m_array = new String[arraySize];
        copy(arr.m_array, arr.m_array + arr.len, m_array);
        return *this;
    }

    String *m_array;
    size_t len;
    size_t arraySize;

    void reallocate() {
        arraySize *= 2;

        auto newArray = new String[arraySize];
        copy(m_array, m_array + len, newArray);
        delete[] m_array;
        m_array = newArray;

    }

    String &operator[](std::size_t index) const {
        return m_array[index];
    }

    void insert(size_t pos, const Array &arr) {
        String *first = lower_bound(m_array, m_array + len, pos, cmpFn);

        unsigned long index = first - m_array;

        size_t newLen = len + arr.len;
        arraySize = newLen + 10;
        auto *newArray = new String[arraySize];

        size_t i = 0;
        for (; i < index; ++i) {    // přidání prvních prvků před pozicí
            newArray[i] = m_array[i];
        }

        bool split = false;
        if (first->position < pos) {
            split = true;
            newArray[index] = m_array[index];
            newArray[index].len = pos - first->position;
            ++i;
        }

        size_t newPos = pos;
        for (size_t j = 0; j < arr.len; ++j, ++i) {  // přidání nových řetězců
            newArray[i] = arr[j];
            newArray[i].position = newPos;
            newPos += newArray[i].len;
        }
        //přidat případně rozdělený string
        newArray[i] = m_array[index];
        if (split) {
            newArray[i].offset += newArray[index].len;
            newArray[i].len -= newArray[index].len;
            newArray[i].position = newPos;
            newPos += newArray[i].len;
            newLen += 1;
        }
        ++i;
        for (size_t j = index + 1; j < len; ++j, ++i) {  // přidání zbytku
            newArray[i] = m_array[j];
            newArray[i].position = newPos;
            newPos += newArray[i].len;
        }

        delete[] m_array;
        m_array = newArray;
        len = newLen;
    }

    // vrací počet smazaných znaků
    void Delete(size_t from, size_t lenStr) {
        size_t lastPos = from + lenStr;
        String *first = lower_bound(m_array, m_array + len, from, cmpFn);
        String *last = lower_bound(m_array, m_array + len, lastPos, cmpFn);

        String lastString = String(*last);
        size_t startI = first - m_array;
        size_t endI = last - m_array;


        if (first->position < from) {
            first->len = from - first->position;

            ++startI;
        }

        if (lastString.position < lastPos) {     // asi bude vždy true
            if (lastString.position + lastString.len != lastPos) { // prvek se nemá smazat
                String secondStart;
                if (startI > endI) { //je potřeba přidak prvek navíc
                    if (len == arraySize) reallocate();
                    secondStart = m_array[startI];
                }
                m_array[startI] = lastString;

                size_t posDiff = lastPos - lastString.position;
                m_array[startI].offset += posDiff;
                m_array[startI].len -= posDiff;
                if (startI > 0) {
                    m_array[startI].position = m_array[startI - 1].position + m_array[startI - 1].len;
                } else {
                    m_array[startI].position = 0;
                }
                ++startI;
                ++endI;

                if (secondStart.data) { // byl přidán prvek navíc, původní z té pozice je v secondStart
                    size_t i = startI;
                    size_t newPos = m_array[i - 1].position + m_array[i - 1].len;
                    String tmp;
                    for (; i < len + 1; ++i) {
                        tmp = m_array[i];
                        m_array[i] = secondStart;
                        m_array[i].position = newPos;
                        newPos += m_array[i].len;
                        secondStart = tmp;
                    }

                    len = i;
                    return;
                }
            } else {
                ++endI; // prvek se má smazat
            }
        }
        size_t i = startI;
        size_t newPos = startI > 0 ? m_array[i - 1].position + m_array[i - 1].len : 0;
        for (size_t j = endI; j < len; ++i, j++) {
            m_array[i] = m_array[j];
            m_array[i].position = newPos;
            newPos += m_array[i].len;
        }

        len = i;
    }
};

class CPatchStr {
public:
    CPatchStr() : strLen(0) {
        //array = Array();
    }

    // copy constructor
    CPatchStr(const char *str) : strLen(0) {
        //array = Array();
        String newString(str);
        strLen += newString.len;
        array.push(newString);
    }

    CPatchStr(const CPatchStr &cpatch) : strLen(0) {
        array = Array(cpatch.array);
        strLen = cpatch.strLen;

    }

    // destructor
    // operator =
    CPatchStr &operator=(const CPatchStr &cpatch) {
        if (this == &cpatch) return *this;

        strLen = cpatch.strLen;
        array = cpatch.array;

        return *this;
    }

    CPatchStr SubStr(size_t from,
                     size_t len) const;

    CPatchStr &Append(const CPatchStr &src);

    CPatchStr &Append(const char *src);

    CPatchStr &Append(String &src);

    CPatchStr &Insert(size_t pos,
                      const CPatchStr &src);

    CPatchStr &Delete(size_t from,
                      size_t len);

    char *ToStr(void) const;

    size_t Length();

    Array array;

    unsigned long strLen;

    friend std::ostream &operator<<(std::ostream &stream, const CPatchStr &cpatch) {
        stream << cpatch.ToStr();
        return stream;
    }
};

size_t CPatchStr::Length() {
    return strLen;
}

CPatchStr &CPatchStr::Append(const CPatchStr &src) {
    array.push(src.array);
    strLen += src.strLen;
    return *this;
}

CPatchStr &CPatchStr::Append(const char *src) {
    String newString(src);
    strLen += newString.len;
    array.push(newString);
    return *this;
}

CPatchStr &CPatchStr::Append(String &str) {
    strLen += str.len;
    array.push(str);
    return *this;
}


char *CPatchStr::ToStr() const {
    char *charArray = new char[strLen + 1];
    unsigned long acc = 0;
    for (size_t i = 0; i < array.len; ++i) {
        String str = array[i];
        strncpy(charArray + acc, str.data->get_c() + str.offset, str.len);
        acc += str.len;
    }
    charArray[strLen] = '\0';
    return charArray;
}

CPatchStr &CPatchStr::Delete(size_t from, size_t lenStr) {
    size_t maxPosition = from + lenStr;
    if (maxPosition > strLen) throw InvalidIndexException();

    if (lenStr == 0)
        return *this;

    array.Delete(from, lenStr);
    strLen -= lenStr;

    return *this;
}

CPatchStr CPatchStr::SubStr(size_t from, size_t lenStr) const {
    size_t maxPosition = from + lenStr;
    if (maxPosition > strLen) throw InvalidIndexException();

    CPatchStr newPatch;
    // newPatch.array.strRealLen = lenStr;
    String *first = lower_bound(array.m_array, array.m_array + array.len, from, cmpFn);

    unsigned long i = first - array.m_array;

    bool firstRun = true;
    for (; i < array.len; ++i) {
        String str = array[i];

        if (firstRun) {
            auto newStr = String(str);
            size_t offsetDiff = from - str.position;
            newStr.offset += offsetDiff;

            newStr.len -= offsetDiff;
            if (newStr.len > lenStr) {
                newStr.len = lenStr;
            }
            newPatch.Append(newStr);
            firstRun = !firstRun;
        } else if (str.position <= maxPosition && maxPosition < str.position + str.len) { // konec
            auto newStr = String(str);
            newStr.len = maxPosition - str.position;
            newPatch.Append(newStr);
            break;
        } else if (str.position + str.len <= maxPosition) { // střed
            auto newStr = String(str);
            newPatch.Append(newStr);
        } else { // prošlo firstRun a dál už nic nechci
            break;
        }

    }

    return newPatch;
}

CPatchStr &CPatchStr::Insert(size_t pos, const CPatchStr &src) {
    if (pos == strLen) {
        return Append(src);
    }
    if (pos > strLen) throw InvalidIndexException();

    array.insert(pos, src.array);
    strLen += src.strLen;
    return *this;
}

#ifndef __PROGTEST__

bool stringMatch(char *str,
                 const char *expected) {
    bool res = strcmp(str, expected) == 0;
    delete[] str;
    return res;
}

int main(void) {
    char tmpStr[100];

    CPatchStr a ( "test" );
    assert ( stringMatch ( a . ToStr (), "test" ) );
    strncpy ( tmpStr, " da", sizeof ( tmpStr ) );
    a . Append ( tmpStr );
    assert ( stringMatch ( a . ToStr (), "test da" ) );
    strncpy ( tmpStr, "ta", sizeof ( tmpStr ) );
    a . Append ( tmpStr );
    assert ( stringMatch ( a . ToStr (), "test data" ) );
    strncpy ( tmpStr, "foo text", sizeof ( tmpStr ) );
    CPatchStr b ( tmpStr );
    assert ( stringMatch ( b . ToStr (), "foo text" ) );
    CPatchStr c ( a );
    assert ( stringMatch ( c . ToStr (), "test data" ) );
    CPatchStr d ( a . SubStr ( 3, 5 ) );
    assert ( stringMatch ( d . ToStr (), "t dat" ) );
    d . Append ( b );
    assert ( stringMatch ( d . ToStr (), "t datfoo text" ) );
    d . Append ( b . SubStr ( 3, 4 ) );
    assert ( stringMatch ( d . ToStr (), "t datfoo text tex" ) );
    c . Append ( d );
    assert ( stringMatch ( c . ToStr (), "test datat datfoo text tex" ) );
    c . Append ( c );
    assert ( stringMatch ( c . ToStr (), "test datat datfoo text textest datat datfoo text tex" ) );
    d . Insert ( 2, c . SubStr ( 6, 9 ) );
    assert ( stringMatch ( d . ToStr (), "t atat datfdatfoo text tex" ) );
    b = "abcdefgh";
    assert ( stringMatch ( b . ToStr (), "abcdefgh" ) );
    assert ( stringMatch ( b . ToStr (), "abcdefgh" ) );
    assert ( stringMatch ( d . ToStr (), "t atat datfdatfoo text tex" ) );
    assert ( stringMatch ( d . SubStr ( 4, 8 ) . ToStr (), "at datfd" ) );
    assert ( stringMatch ( b . SubStr ( 2, 6 ) . ToStr (), "cdefgh" ) );
    try
    {
        b . SubStr ( 2, 7 ) . ToStr ();
        assert ( "Exception not thrown" == NULL );
    }
    catch ( InvalidIndexException & e )
    {
    }
    catch ( ... )
    {
        assert ( "Invalid exception thrown" == NULL );
    }
    a . Delete ( 3, 5 );
    assert ( stringMatch ( a . ToStr (), "tesa" ) );
    return 0;
}

#endif /* __PROGTEST__ */
