
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

String * binary_Search_rec(String arr[], size_t l, size_t r, size_t pos)
{
    if (r >= l) {
        int mid = l + (r - l) / 2;

        // If the element is present at the middle
        // itself
        if (arr[mid].position <= pos &&  pos < arr[mid].position + arr[mid].len)
            return &(arr[mid]);

        // If element is smaller than mid, then
        // it can only be present in left subarray
        if (arr[mid].position > pos)
            return binary_Search_rec(arr, l, mid - 1, pos);

        // Else the element can only be present
        // in right subarray
        return binary_Search_rec(arr, mid + 1, r, pos);
    }

    // We reach here when element is not
    // present in array
    return NULL;
}

String * binary_search(String arr[], size_t len,size_t pos) {
    return binary_Search_rec(arr, 0, len, pos);

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
            newArray[i + j] = array1[j];
            newArray[i + j].position = position;
            position += newArray[i + j].len;
        }

        delete[] m_array;
        len = len + array1.len;
        m_array = newArray;
    }

    Array() {
        len = 0;
        arraySize = 1;
        m_array = new String[arraySize];
    }

    Array(const Array &arr) {
        len = arr.len;
        arraySize = arr.arraySize;
        m_array = new String[arraySize];
        copy(arr.m_array, arr.m_array + arr.len, m_array);
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
        String *first = binary_search(m_array, len, pos);

        unsigned long index = first - m_array;

        size_t newLen = len + arr.len;
        arraySize = newLen + 10;
        auto *newArray = new String[arraySize];

        size_t i = 0;
        for (; i < index; ++i) {    // přidání prvních prvků před pozicí
            newArray[i] = m_array[i];
        }

        size_t diffLenSplit = 0;
        bool cuted = false;
        if (first->position < pos) {
            cuted = true;
            newArray[index] = m_array[index];
            diffLenSplit =  m_array[index].len - (pos - first->position);
            newArray[index].len = pos - first->position;
            if (newArray[index].len == 0) throw InvalidIndexException();
            ++i;
        }

        size_t newPos = pos;
        for (size_t j = 0; j < arr.len; ++j, ++i) {  // přidání nových řetězců
            newArray[i] = arr[j];
            newArray[i].position = newPos;
            newPos += newArray[i].len;
        }
        //přidat případně rozdělený string

        if (diffLenSplit > 0) {
            newArray[i] = m_array[index];
            newArray[i].offset += newArray[index].len;
            newArray[i].len -= newArray[index].len;
            if (newArray[i].len == 0) throw InvalidIndexException();
            newArray[i].position = newPos;
            newPos += newArray[i].len;
            newLen += 1;
            ++i;
        } else if (!cuted) {
            newArray[i] = m_array[index];
            newArray[i].position = newPos;
            newPos += newArray[i].len;
            ++i;
        }

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
        String *first = binary_search(m_array, len, from);
        String *last = binary_search(m_array, len, lastPos - 1);

        String lastString = String(*last);
        size_t startI = first - m_array;
        size_t endI = last - m_array;


        bool firstSplit = false;
        if (first->position < from) {
            first->len = from - first->position;
            ++startI;

            firstSplit = true;
        }

        bool secSplit = false;
        String secondStart;
        if (lastPos < lastString.position + lastString.len) {
            if (startI == arraySize) reallocate();
            secondStart = m_array[startI];
            m_array[startI] = lastString;

            size_t posDiff = lastPos - lastString.position;
            m_array[startI].offset += posDiff;
            m_array[startI].len -= posDiff;
            m_array[startI].position = startI > 0 ? m_array[startI - 1].position + m_array[startI - 1].len : 0;
            ++startI;
            ++endI;
            secSplit = true;
        }else ++endI;

        if (first == last && ((!firstSplit && secSplit) || (firstSplit && !secSplit))) {} // useklo se z jednoho stringu
        else if (first == last && secSplit && firstSplit) {   // rozdělení stringu na dva - první a druhý jsou upraveny, druhý původní je v secondStart
            if (len == arraySize) reallocate();
            size_t newPos = m_array[startI - 1].position + m_array[startI - 1].len;
            String tmp;
            size_t i = startI;
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


        size_t newPos = startI > 0 ? m_array[startI - 1].position + m_array[startI - 1].len : 0;
        for (size_t j = endI; j < len; ++startI, ++j) {
            m_array[startI] = m_array[j];
            m_array[startI].position = newPos;
            newPos += m_array[startI].len;
        }

        len = startI;
    }
};

class CPatchStr {
public:
    CPatchStr() : strLen(0) {}

    CPatchStr(const char *str) : strLen(0) {
        String newString(str);
        strLen += newString.len;
        array.push(newString);
    }

    CPatchStr(const CPatchStr &cpatch) : strLen(0) {
        array = Array(cpatch.array);
        strLen = cpatch.strLen;

    }

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

    friend std::ostream& operator<< (std::ostream& stream, const CPatchStr& str) {
        size_t pos = 0;
        for (int i = 0; i < str.array.len; ++i) {
            if (str.array[i].len == 0) throw InvalidIndexException();
            if (str.array[i].position != pos) throw InvalidIndexException();

            stream << str.ToStr();
            pos += str.array[i].len;
        }
        if (str.strLen != pos)  throw InvalidIndexException();
        stream << endl;
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

//strLen = 10 - dékla aktuální
//from = 0 - od
//lenStr = 10 - dékla odstranění
CPatchStr &CPatchStr::Delete(size_t from, size_t lenStr) {
    if (from > strLen || lenStr > strLen) throw InvalidIndexException();
    if (strLen - from < lenStr) throw InvalidIndexException();

    size_t maxPosition = from + lenStr;
    if (maxPosition > strLen) throw InvalidIndexException();

    if (lenStr == 0)
        return *this;

    array.Delete(from, lenStr);
    strLen -= lenStr;

    return *this;
}

CPatchStr CPatchStr::SubStr(size_t from, size_t lenStr) const {
    if (from > strLen || lenStr > strLen) throw InvalidIndexException();
    if (strLen - from < lenStr) throw InvalidIndexException();

    size_t maxPosition = from + lenStr;
    if (maxPosition > strLen) throw InvalidIndexException();

    CPatchStr newPatch;
    // newPatch.array.strRealLen = lenStr;
    String *first = binary_search(array.m_array, array.len, from);

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


bool stringMatch(char *str,
                 const char *expected) {
    bool res = strcmp(str, expected) == 0;
    delete[] str;
    return res;
}

void run147A() {
    /* testing appending
 */

    CPatchStr x("test");
    assert(x.SubStr(2, 0).Length() == 0);
    string s = "test";
    x.Append("ing");
    s.append("ing");
    x.Append(" this");
    s.append(" this");
    x.Append(" append function");
    s.append(" append function");
    x.Append(", hopefully");
    s.append(", hopefully");
    x.Append(" its going okay");
    s.append(" its going okay");
    assert (stringMatch(x.ToStr(), s.c_str()));

    CPatchStr x2("another");
    string s2("another");
    x2.Append(" string");
    s2.append(" string");
    x2.Append(x);
    s2.append(s);
    x2.Append(x2);
    s2.append(s2);
    assert (stringMatch(x2.ToStr(), s2.c_str()));
    x.Append("a");
    s.append("a");
    x.Append("b");
    s.append("b");
    x.Append("c");
    s.append("c");
    x.Append("d");
    s.append("d");
    x.Append("e");
    s.append("e");
    x.Append("f");
    s.append("f");
    x.Append("g");
    s.append("g");
    x.Append("h");
    s.append("h");
    assert (stringMatch(x.ToStr(), s.c_str()));
    x.Append("");
    s.append("");
    x.Append("finaltest");
    s.append("finaltest");
    assert (stringMatch(x.ToStr(), s.c_str()));

    /* 147 allocs
     */
}

void run2206A() {
    /* testing insertion
     */

    CPatchStr x;
    string s;

    x.Append("testtest");
    s.append("testtest");
    x.Insert(4, "ing");
    s.insert(4, "ing");
    x.Insert(0, "Too");
    s.insert(0, "Too");
    assert (stringMatch(x.ToStr(), s.c_str()));
    x.Insert(5, "");
    s.insert(5, "");
    assert (stringMatch(x.ToStr(), s.c_str()));
    x.Insert(0, "a");
    s.insert(0, "a");
    x.Insert(1, "b");
    s.insert(1, "b");
    x.Insert(2, "c");
    s.insert(2, "c");
    x.Insert(3, "d");
    s.insert(3, "d");
    x.Insert(4, "e");
    s.insert(4, "e");
    x.Insert(5, "f");
    s.insert(5, "f");
    x.Insert(6, "g");
    s.insert(6, "g");
    x.Insert(7, "h");
    s.insert(7, "h");
    x.Insert(x.Length(), "hello");
    s.insert(s.length(), "hello");
    assert (stringMatch(x.ToStr(), s.c_str()));
    try {
        x.Insert(x.Length() + 1, "hello");
        assert ("No exception thrown" == NULL);
    }
    catch (const InvalidIndexException &e) {
    }
    catch (...) {
        assert ("Invalid exception thrown" == NULL);
    }
    CPatchStr x2("cool");
    string s2("cool");
    x2.Insert(2, "FOO");
    s2.insert(2, "FOO");
    x2.Insert(4, x);
    s2.insert(4, s);
    x2.Insert(12, x2);
    s2.insert(12, s2);
    x2.Insert(x2.Length(), x2);
    s2.insert(s2.length(), s2);
    x2.Insert(x2.Length(), x2);
    s2.insert(s2.length(), s2);
    x2.Insert(x2.Length(), x2);
    s2.insert(s2.length(), s2);
    x2.Insert(x2.Length(), x2);
    s2.insert(s2.length(), s2);
    assert (stringMatch(x.ToStr(), s.c_str()));

    /* 2 260 allocs
     */
}

void run2345A() {
    /* testing deletion, insertion, appending
    */

    CPatchStr x("fubar");
    string s("fubar");
    x.Delete(2, 3);
    s.erase(2, 3);
    assert (stringMatch(x.ToStr(), s.c_str()));
    x = "fubar";
    s = "fubar";
    x.Delete(0, 5);
    s.erase(0, 5);
    assert (stringMatch(x.ToStr(), s.c_str()));
    x = "fubar";
    s = "fubar";
    x.Delete(0, 3);
    s.erase(0, 3);
    assert (stringMatch(x.ToStr(), s.c_str()));
    x = "fubar";
    s = "fubar";
    x.Delete(3, 2);
    s.erase(3, 2);
    assert (stringMatch(x.ToStr(), s.c_str()));
    x = "fubar";
    s = "fubar";
    x.Append("hello");
    s.append("hello");
    x.Append("bleble");
    s.append("bleble");
    x.Append("rumbaba");
    s.append("rumbaba");
    x.Delete(5, 15);
    s.erase(5, 15);
    assert (stringMatch(x.ToStr(), s.c_str()));
    x.Insert(0, "abcdefghijkl");
    s.insert(0, "abcdefghijkl");
    x.Insert(5, "lalala");
    s.insert(5, "lalala");
    x.Delete(0, 4);
    s.erase(0, 4);
    assert (stringMatch(x.ToStr(), s.c_str()));
    x.Append("abc");
    s.append("abc");
    x.Delete(0, x.Length());
    s.erase(0, s.length());
    assert (stringMatch(x.ToStr(), s.c_str()));
    x.Delete(0, 0);
    s.erase(0, 0);
    assert (stringMatch(x.ToStr(), s.c_str()));
    try {
        x.Delete(0, 1);
        assert ("No exception thrown" == NULL);
    }
    catch (const InvalidIndexException &e) {
    }
    catch (...) {
        assert ("Invalid exception thrown" == NULL);
    }
    x.Append("abcde");
    s.append("abcde");
    x.Append(x);
    s.append(s);
    x.Append("abc");
    s.append("abc");
    try {
        x.Delete(10, 15);
        assert ("No exception thrown" == NULL);
    }
    catch (const InvalidIndexException &e) {
    }
    catch (...) {
        assert ("Invalid exception thrown" == NULL);
    }
    try {
        x.Delete(x.Length() + 1, 0);
        assert ("No exception thrown" == NULL);
    }
    catch (const InvalidIndexException &e) {
    }
    catch (...) {
        assert ("Invalid exception thrown" == NULL);
    }
    x.Delete(12, 1);
    s.erase(12, 1);
    assert (stringMatch(x.ToStr(), s.c_str()));

    /* 2 345 allocs
     */
}

void run2453A() {
    /* testing copying, assignments, self insertions and appending
     */
    CPatchStr x1("fubar");
    CPatchStr x2("foobar");
    CPatchStr x3("hello");
    string s1("fubar");
    string s2("foobar");
    string s3("hello");
    x1.Append(x2);
    s1.append(s2);
    x1 = x2;
    s1 = s2;
    assert (stringMatch(x1.ToStr(), s1.c_str()));
    x2.Append(x3);
    s2.append(s3);
    x2.Insert(4, x1);
    s2.insert(4, s1);
    assert (stringMatch(x2.ToStr(), s2.c_str()));
    x3.Append(x2);
    s3.append(s2);
    x1.Insert(0, x3);
    s1.insert(0, s3);
    x1.Insert(4, x1.Append("abc"));
    s1.insert(4, s1.append("abc"));
    assert (stringMatch(x1.ToStr(), s1.c_str()));

    /* 2 453 allocs
     */
}

void run2453v2A() {
    /* testing (SubStr), chaining of commands
     */

    CPatchStr x1("substrtest");
    string s1("substrtest");
    CPatchStr x2(x1.SubStr(3, 3));
    string s2(s1.substr(3, 3));
    assert (stringMatch(x2.ToStr(), s2.c_str()));
    x2.Append("moretesting");
    s2.append("moretesting");
    CPatchStr x3(x2.SubStr(5, 5));
    string s3(s2.substr(5, 5));
    assert (stringMatch(x3.ToStr(), s3.c_str()));
    CPatchStr x4(x1.SubStr(0, 0));
    string s4(s1.substr(0, 0));
    assert (stringMatch(x4.ToStr(), s4.c_str()));
    try {
        CPatchStr x5(x4.SubStr(0, 1));
        assert ("No exception thrown" == NULL);
    }
    catch (const InvalidIndexException &e) {
    }
    catch (...) {
        assert ("Invalid exception thrown." == NULL);
    }
    x1.Append("abc");
    s1.append("abc");
    assert
    (
            stringMatch
                    (
                            x1.Append("def").Insert(3, "hello").SubStr(5, 8).ToStr(),
                            s1.append("def").insert(3, "hello").substr(5, 8).c_str()
                    )
    );
    assert
    (
            stringMatch
                    (
                            x1.SubStr(x1.Length(), 0).ToStr(),
                            s1.substr(s1.length(), 0).c_str()
                    )
    );
    assert
    (
            stringMatch
                    (
                            x1.Delete(3, 5).Insert(3, "brrr").SubStr(4, 6).ToStr(),
                            s1.erase(3, 5).insert(3, "brrr").substr(4, 6).c_str()
                    )
    );

    /* 2 543 allocs
     */
}

void DeleteTest() {
    CPatchStr a("aaaa");
    a.Insert(0, "bbbb ");
    assert (stringMatch(a.ToStr(), "bbbb aaaa"));
    a.Insert(2, " cccc ");
    assert (stringMatch(a.ToStr(), "bb cccc bb aaaa"));
    a.Insert(15, " dddd");
    assert (stringMatch(a.ToStr(), "bb cccc bb aaaa dddd"));
    a.Insert(0, "bbbb ");
    assert (stringMatch(a.ToStr(), "bbbb bb cccc bb aaaa dddd"));
    a.Insert(2, " cccc ");
    assert (stringMatch(a.ToStr(), "bb cccc bb bb cccc bb aaaa dddd"));
    a.Insert(31, " dddd");
    assert (stringMatch(a.ToStr(), "bb cccc bb bb cccc bb aaaa dddd dddd"));
    a.Insert(0, "xxxx ").Insert(41, " yyyy");
    assert (stringMatch(a.ToStr(), "xxxx bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    a.Insert(1, "1111");
    assert (stringMatch(a.ToStr(), "x1111xxx bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    a.Insert(8, "8888");
    assert (stringMatch(a.ToStr(), "x1111xxx8888 bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    cout << a;
    a.Insert(13, "1313");
    assert (stringMatch(a.ToStr(), "x1111xxx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    a.Insert(7, a.SubStr(0, 23));
    assert (stringMatch(a.ToStr(),
                        "x1111xxx1111xxx8888 1313bb cccx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    a.Append("asdfasdf65654564asf");
    a.Append(a);
    a.Append(a);

    string s = "x1111xxx1111xxx8888 1313bb cccx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyyasdfasdf65654564asfx1111xxx1111xxx8888 1313bb cccx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyyasdfasdf65654564asfx1111xxx1111xxx8888 1313bb cccx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyyasdfasdf65654564asfx1111xxx1111xxx8888 1313bb cccx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyyasdfasdf65654564asf";

    cout << a;
    a.Delete(25, 1);
    s.erase(25, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(26, 1);
    s.erase(26, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    auto kebb = a.ToStr();
    auto kelb = s.c_str();
    a.Delete(34, 1);
    s.erase(34, 1);
    auto keb = a.ToStr();
    auto kel = s.c_str();
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(12, 1);
    s.erase(12, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(34, 1);
    s.erase(34, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(6, 1);
    s.erase(6, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(18, 1);
    s.erase(18, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(36, 1);
    s.erase(36, 1);
    cout << a;
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(17, 1);
    s.erase(17, 1);
    auto kek = a.ToStr();
    auto kes = s.c_str();
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(5, 1);
    s.erase(5, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(35, 1);
    s.erase(35, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(22, 1);
    s.erase(22, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(1, 1);
    s.erase(1, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(14, 1);
    s.erase(14, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(6, 1);
    s.erase(6, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(0, 1);
    s.erase(0, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(9, 1);
    s.erase(9, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(36, 1);
    s.erase(36, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(37, 1);
    s.erase(37, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(11, 1);
    s.erase(11, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(37, 1);
    s.erase(37, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(35, 1);
    s.erase(35, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(2, 1);
    s.erase(2, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(38, 1);
    s.erase(38, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(25, 1);
    s.erase(25, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(5, 1);
    s.erase(5, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(15, 1);
    s.erase(15, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(40, 1);
    s.erase(40, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(31, 1);
    s.erase(31, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(37, 1);
    s.erase(37, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(21, 1);
    s.erase(21, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(12, 1);
    s.erase(12, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(25, 1);
    s.erase(25, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(4, 1);
    s.erase(4, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(39, 1);
    s.erase(39, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(38, 1);
    s.erase(38, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(31, 1);
    s.erase(31, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(22, 1);
    s.erase(22, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(31, 1);
    s.erase(31, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(10, 1);
    s.erase(10, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(19, 1);
    s.erase(19, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(23, 1);
    s.erase(23, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(37, 1);
    s.erase(37, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(24, 1);
    s.erase(24, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(13, 1);
    s.erase(13, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(25, 1);
    s.erase(25, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(19, 1);
    s.erase(19, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(36, 1);
    s.erase(36, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(36, 1);
    s.erase(36, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(5, 1);
    s.erase(5, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(25, 1);
    s.erase(25, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(32, 1);
    s.erase(32, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(24, 1);
    s.erase(24, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(26, 1);
    s.erase(26, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(0, 1);
    s.erase(0, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(31, 1);
    s.erase(31, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(21, 1);
    s.erase(21, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(10, 1);
    s.erase(10, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(4, 1);
    s.erase(4, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(15, 1);
    s.erase(15, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(29, 1);
    s.erase(29, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(2, 1);
    s.erase(2, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(16, 1);
    s.erase(16, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(20, 1);
    s.erase(20, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(28, 1);
    s.erase(28, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(7, 1);
    s.erase(7, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(23, 1);
    s.erase(23, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(10, 1);
    s.erase(10, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(12, 1);
    s.erase(12, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(21, 1);
    s.erase(21, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));

    cout << a;
    a.Delete(13, 6);
    s.erase(13, 6);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(4, 16);
    s.erase(4, 16);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(20, 4);
    s.erase(20, 4);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(15, 4);
    s.erase(15, 4);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(19, 11);
    s.erase(19, 11);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(0, 17);
    s.erase(0, 17);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(12, 3);
    s.erase(12, 3);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(18, 14);
    s.erase(18, 14);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(20, 12);
    s.erase(20, 12);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(12, 6);
    s.erase(12, 6);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(1, 1);
    s.erase(1, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(18, 17);
    s.erase(18, 17);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(8, 9);
    s.erase(8, 9);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(6, 1);
    s.erase(6, 1);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(13, 4);
    s.erase(13, 4);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(8, 15);
    s.erase(8, 15);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(15, 12);
    s.erase(15, 12);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(18, 18);
    s.erase(18, 18);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(4, 20);
    s.erase(4, 20);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(11, 5);
    s.erase(11, 5);
    cout << a;
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(9, 5);
    s.erase(9, 5);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(18, 19);
    s.erase(18, 19);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(10, 3);
    s.erase(10, 3);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(9, 2);
    s.erase(9, 2);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(4, 9);
    s.erase(4, 9);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(15, 2);
    s.erase(15, 2);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(12, 2);
    s.erase(12, 2);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(17, 4);
    s.erase(17, 4);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(19, 11);
    s.erase(19, 11);
    assert(stringMatch(a.ToStr(), s.c_str()));
    a.Delete(6, 18);
    s.erase(6, 18);
    assert(stringMatch(a.ToStr(), s.c_str()));
}

void MC_getStrTest() {
    CPatchStr a;
    a = "aaaa";
    a.Append(" bbbb");
    a.Append(" cccc");
    a.Append(" dddd");
    a.Append(" eeee");
    a.Append(" ffff");
    a.Append(" gggg");
    assert (stringMatch(a.ToStr(), "aaaa bbbb cccc dddd eeee ffff gggg"));
    cout << "\033[1;42m Test MC_GetStr 1 dokončen \033[0m" << endl << endl;
    a.Insert(0, "1111 ");
    assert (stringMatch(a.ToStr(), "1111 aaaa bbbb cccc dddd eeee ffff gggg"));
    cout << "\033[1;42m Test MC_GetStr 2 dokončen \033[0m" << endl << endl;
    a = "aaaa";
    a.Append(" bbbb");
    a.Append(" cccc");
    a.Append(" dddd");
    a.Append(" eeee");
    a.Append(" ffff");
    a.Append(" gggg");
    a.Insert(3, "1111");
    assert (stringMatch(a.ToStr(), "aaa1111a bbbb cccc dddd eeee ffff gggg"));
    cout << "\033[1;42m Test MC_GetStr 3 dokončen \033[0m" << endl << endl;
    a = "aaaa";
    a.Append(" bbbb");
    a.Append(" cccc");
    a.Append(" dddd");
    a.Append(" eeee");
    a.Append(" ffff");
    a.Append(" gggg");
    a.Insert(4, "1111");
    assert (stringMatch(a.ToStr(), "aaaa1111 bbbb cccc dddd eeee ffff gggg"));
    cout << "\033[1;42m Test MC_GetStr 4 dokončen \033[0m" << endl << endl;
    a = "aaaa";
    a.Append(" bbbb");
    a.Append(" cccc");
    a.Append(" dddd");
    a.Append(" eeee");
    a.Append(" ffff");
    a.Append(" gggg");
    a.Insert(5, "1111");
    assert (stringMatch(a.ToStr(), "aaaa 1111bbbb cccc dddd eeee ffff gggg"));
    cout << "\033[1;42m Test MC_GetStr 4 dokončen \033[0m" << endl << endl;
    a = "aaaa";
    a.Append(" bbbb");
    a.Append(" cccc");
    a.Append(" dddd");
    a.Insert(19, "1111");
    assert (stringMatch(a.ToStr(), "aaaa bbbb cccc dddd1111"));
    cout << "\033[1;42m Test MC_GetStr 5 dokončen \033[0m" << endl << endl;
    cout << "\033[1;42m\n\n      testy mezních hodnot dokončeny     \n\033[0m" << endl << endl;
}

void MC_otherTests() {
    CPatchStr a("abcdefghijklmnopqrstuvwxyz");
    //auto kek = a.Delete(2, 15).ToStr();
    a.Insert(10, a.Delete(2, 15));
    assert (stringMatch(a.ToStr(), "abrstuvwxyabrstuvwxyzz"));
    cout << "\033[1;42m Test MC_Others 1 dokončen \033[0m" << endl << endl;
    a.Append(a.SubStr(10, 12));
    assert (stringMatch(a.ToStr(), "abrstuvwxyabrstuvwxyzzabrstuvwxyzz"));
    cout << "\033[1;42m Test MC_Others 2 dokončen \033[0m" << endl << endl;
    CPatchStr b("123456");
    a.Append(b);
    assert (stringMatch(a.ToStr(), "abrstuvwxyabrstuvwxyzzabrstuvwxyzz123456"));
    cout << "\033[1;42m Test MC_Others 3 dokončen \033[0m" << endl << endl;
    b.Delete(0, 6);
    assert (stringMatch(b.ToStr(), ""));
    cout << "\033[1;42m Test MC_Others 4 dokončen \033[0m" << endl << endl;
    a.Append(b);
    assert (stringMatch(a.ToStr(), "abrstuvwxyabrstuvwxyzzabrstuvwxyzz123456"));
    cout << "\033[1;42m Test MC_Others 5 dokončen \033[0m" << endl << endl;
    a.Delete(0, 40);
    assert (stringMatch(a.ToStr(), ""));
    cout << "\033[1;42m Test MC_Others 6 dokončen \033[0m" << endl << endl;
    cout << "\033[1;42m\n\n      další testy dokončeny     \n\033[0m" << endl << endl;
}

void MC_allocationTest() {
    CPatchStr a("");
    const int RAND_LEN = 30000;
    char randSeed[RAND_LEN];
    for (int i = 0; i < RAND_LEN - 1; i++) {
        randSeed[i] = 'a' + (int) (rand() * 26.0 / RAND_MAX);
    }
    randSeed[RAND_LEN - 1] = 0;
    for (int i = 0; i != 50000; ++i) {
        a.Append(randSeed);
    }
    cout << "\033[1;42m\n\n      testy alokace dokončeny     \n\033[0m" << endl << endl;
}

void MC_deleteTest() {
    CPatchStr a("abcdefghijklmnopqrstuvwxyz");

//odebírání v rámci jedné části
//odebírání ze začátku
    a.Delete(0, 3);
    assert (stringMatch(a.ToStr(), "defghijklmnopqrstuvwxyz"));
//odebírání z prostředka
    a.Delete(5, 5);
    assert (stringMatch(a.ToStr(), "defghnopqrstuvwxyz"));
//odebírání z konce
    a.Delete(12, 6);
    assert (stringMatch(a.ToStr(), "defghnopqrst"));
//odebírání napříč částmi
    a = "aaaa";
    a.Append(" bbbb");
    a.Append(" cccc");
    a.Append(" dddd");
    a.Append(" eeee");
    a.Append(" ffff");
    a.Append(" gggg");
    a.Delete(2, 5);
    assert (stringMatch(a.ToStr(), "aabb cccc dddd eeee ffff gggg"));
    a.Delete(1, 16); // TODO TRIPLE MEM LEAK
    assert (stringMatch(a.ToStr(), "aee ffff gggg"));
    a.Append(" bbbb");
    a.Append(" cccc");
    a.Append(" dddd");
    a.Append(" eeee");
    assert (stringMatch(a.ToStr(), "aee ffff gggg bbbb cccc dddd eeee"));
    a.Delete(2, 12).Delete(5, 10).Delete(0, 8);
    assert (stringMatch(a.ToStr(), "eee"));
//odebrání prázdné části
    a.Delete(0, 0).Delete(1, 0).Delete(2, 0);
    assert (stringMatch(a.ToStr(), "eee"));
}

void MC_assignTest() {
    //přiřazení prázdného řetězce do prázdného řetězce
    CPatchStr a, b;
    a = "";
    b = "";
    a = b;
    a = a;
    assert (stringMatch(a.ToStr(), ""));
    cout << "\033[1;42m Test MC_Assign 1 dokončen \033[0m" << endl << endl;
    assert (stringMatch(b.ToStr(), ""));
    cout << "\033[1;42m Test MC_Assign 2 dokončen \033[0m" << endl << endl;
//přiřazení v rámci zakládání objektu
    a = "aaaa";
    CPatchStr c = a;
    assert (stringMatch(c.ToStr(), "aaaa"));
    cout << "\033[1;42m Test MC_Assign 3 dokončen \033[0m" << endl << endl;
    c = c;
    assert (stringMatch(c.ToStr(), "aaaa"));
    cout << "\033[1;42m Test MC_Assign 4 dokončen \033[0m" << endl << endl;
//řetězení přiřazení
    a = b = c = "cccc";
    assert (stringMatch(a.ToStr(), "cccc"));
    cout << "\033[1;42m Test MC_Assign 5 dokončen \033[0m" << endl << endl;
    assert (stringMatch(b.ToStr(), "cccc"));
    cout << "\033[1;42m Test MC_Assign 6 dokončen \033[0m" << endl << endl;
    assert (stringMatch(c.ToStr(), "cccc"));
    cout << "\033[1;42m Test MC_Assign 7 dokončen \033[0m" << endl << endl;
//přiřazení novho objektu do existujícího
    c = CPatchStr();
    assert (stringMatch(c.ToStr(), ""));
    cout << "\033[1;42m Test MC_Assign 8 dokončen \033[0m" << endl << endl;
    cout << "\033[1;42m\n\n      testy přiřazování dokončeny     \n\033[0m" << endl << endl;
}

void MC_insertTest() {
    CPatchStr a("aaaa");
//vkládání na začátek části řetězce
    a.Insert(0, "bbbb ");
    assert (stringMatch(a.ToStr(), "bbbb aaaa"));
    cout << "\033[1;42m Test MC_Insert 1 dokončen \033[0m" << endl << endl;
//vkládání doprostřed části řetězce
    a.Insert(2, " cccc ");
    assert (stringMatch(a.ToStr(), "bb cccc bb aaaa"));
    cout << "\033[1;42m Test MC_Insert 2 dokončen \033[0m" << endl << endl;
//vkládání na konec části řetězce
    a.Insert(15, " dddd");
    assert (stringMatch(a.ToStr(), "bb cccc bb aaaa dddd"));
    cout << "\033[1;42m Test MC_Insert 3 dokončen \033[0m" << endl << endl;
//vkládání na začátek části řetězce
    a.Insert(0, "bbbb ");
    assert (stringMatch(a.ToStr(), "bbbb bb cccc bb aaaa dddd"));
    cout << "\033[1;42m Test MC_Insert 4 dokončen \033[0m" << endl << endl;
//vkládání doprostřed části řetězce
    a.Insert(2, " cccc ");
    assert (stringMatch(a.ToStr(), "bb cccc bb bb cccc bb aaaa dddd"));
    cout << "\033[1;42m Test MC_Insert 5 dokončen \033[0m" << endl << endl;
//vkládání na konec části řetězce
    a.Insert(31, " dddd");
    assert (stringMatch(a.ToStr(), "bb cccc bb bb cccc bb aaaa dddd dddd"));
    cout << "\033[1;42m Test MC_Insert 6 dokončen \033[0m" << endl << endl;
//řetězení vkládání 1) na začátek, 2) na konec
    a.Insert(0, "xxxx ").Insert(41, " yyyy");
    assert (stringMatch(a.ToStr(), "xxxx bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    cout << "\033[1;42m Test MC_Insert 7 dokončen \033[0m" << endl << endl;
//náhodně určené vkládání
    a.Insert(1, "1111");
    assert (stringMatch(a.ToStr(), "x1111xxx bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    cout << "\033[1;42m Test MC_Insert 8 dokončen \033[0m" << endl << endl;
    a.Insert(8, "8888");
    assert (stringMatch(a.ToStr(), "x1111xxx8888 bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    cout << "\033[1;42m Test MC_Insert 9 dokončen \033[0m" << endl << endl;
    a.Insert(13, "1313");
    assert (stringMatch(a.ToStr(), "x1111xxx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    cout << "\033[1;42m Test MC_Insert 10 dokončen \033[0m" << endl << endl;
//náročnější testy vkládající jiný řetězec
    a.Insert(7, a.SubStr(0, 23));
    assert (stringMatch(a.ToStr(),
                        "x1111xxx1111xxx8888 1313bb cccx8888 1313bb cccc bb bb cccc bb aaaa dddd dddd yyyy"));
    cout << "\033[1;42m Test MC_Insert 11 dokončen \033[0m" << endl << endl;
    cout << "\033[1;42m\n\n      testy vkládání dokončeny     \n\033[0m" << endl << endl;
}

void MC_appendTest() {
    //stadnartní sloučení tří řetězců
    CPatchStr a("aaaa");
    CPatchStr b;
    b = " bbbb";
    CPatchStr c(" cccc ");
    a.Append(b).Append(c);
    assert (stringMatch(a.ToStr(), "aaaa bbbb cccc "));
    cout << "\033[1;42m Test MC_Append 1 dokončen \033[0m" << endl << endl;
//připojení řetězce do sebe sama
    a.Append(a);
    assert (stringMatch(a.ToStr(), "aaaa bbbb cccc aaaa bbbb cccc "));
    cout << "\033[1;42m Test MC_Append 2 dokončen \033[0m" << endl << endl;
//připojení nového, prázdného řetězce
    a.Append(CPatchStr()).Append(CPatchStr()).Append(CPatchStr()).Append(CPatchStr());
    assert (stringMatch(a.ToStr(), "aaaa bbbb cccc aaaa bbbb cccc "));
    cout << "\033[1;42m Test MC_Append 3 dokončen \033[0m" << endl << endl;
//připojení prázdného řetězce do přázdnéoh řetězce
    CPatchStr e;
    e.Append(CPatchStr()).Append(CPatchStr()).Append(CPatchStr()).Append(CPatchStr());
    assert (stringMatch(e.ToStr(), ""));
    cout << "\033[1;42m Test MC_Append 4 dokončen \033[0m" << endl << endl;
//připojení prázdného řetězce do sebe sama
    e.Append(e).Append(e).Append(e);
    assert (stringMatch(e.ToStr(), ""));
    cout << "\033[1;42m Test MC_Append 5 dokončen \033[0m" << endl << endl;
//připojení běžného řetězce k prázdnému řetězci
    e.Append(a);
    assert (stringMatch(e.ToStr(), "aaaa bbbb cccc aaaa bbbb cccc "));
    cout << "\033[1;42m Test MC_Append 6 dokončen \033[0m" << endl << endl;
    cout << "\033[1;42m\n\n      testy připojování dokončeny     \n\033[0m" << endl << endl;

}

void MC_Tests() {
    MC_appendTest();
    MC_insertTest();
    MC_assignTest();
    MC_deleteTest();
    MC_allocationTest();
    MC_otherTests();
    MC_getStrTest();
    cout << "\033[1;42m\n\n\n      MC testy dokončeny     \n\n\033[0m" << endl << endl;
}

void custom() {
    CPatchStr g("io");
    string x = "ioabcdef";

    g.Append("abcdef");
    assert (stringMatch(g.ToStr(), x.c_str()));

    g.Append("abbbb");
    x += "abbbb";

    g.Delete(3, 1);
    x.erase(3, 1);
    assert (stringMatch(g.ToStr(), x.c_str()));

    g.Append("je");
    g.Append(" vazne");
    g.Append(" s");
    g.Append("u");
    g.Append("p");
    g.Append("e");
    g.Append("r!");

    //assert (stringMatch(g.ToStr(), "iot je vazne super!"));
}

void	test123456()
{
    CPatchStr a("test");
    for( size_t i = 0; i < 100; i ++ )
    {
        a . Insert( i, a );
    }
}

int main() {
    //custom();
   /* DeleteTest();
    run147A();
    run2206A();
    run2345A();
    run2453A();
    run2453v2A();
    DeleteTest();
    MC_getStrTest();
    MC_otherTests();
    MC_allocationTest();
    MC_deleteTest();
    MC_assignTest();
    MC_insertTest();
    MC_appendTest();
    MC_Tests();*/
    test123456();

}
