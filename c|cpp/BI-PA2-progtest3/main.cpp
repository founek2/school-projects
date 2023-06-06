#ifndef __PROGTEST__

#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <cctype>
#include <ctime>
#include <climits>
#include <cmath>
#include <cassert>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>
#include <list>
#include <algorithm>
#include <functional>
#include <memory>

using namespace std;

class InvalidRangeException {
};

#endif /* __PROGTEST__ */

// uncomment if your code implements initializer lists
#define EXTENDED_SYNTAX

class CRangeList;

class CRange {
public:
    CRange(long long lo, long long hi) {
        if (lo > hi)
            throw InvalidRangeException();

        low = lo;
        high = hi;
    }

    CRange &operator=(const CRange &other) // copy assignment
    {
        if (this != &other) { // self-assignment check expected
            this->low = other.low;
            this->high = other.high;
        }
        return *this;
    }

    long long high;
    long long low;

    friend CRangeList operator+(const CRange &a, const CRange &b);

    friend CRangeList operator+(const CRangeList &list, const CRange &b);

    //operator -
    friend CRangeList operator-(const CRange &a, const CRange &b);

    friend CRangeList operator-(const CRangeList &list, const CRange &b);

    friend bool operator<(const CRange &range1, const CRange &range2) {
        return range1.high < range2.high;
    }

    friend std::ostream &operator<<(std::ostream &stream, const CRange &r);
};

std::ostream &operator<<(std::ostream &stream, const CRange &r) {
    ios_base::fmtflags fl = stream.flags();

    stream.setf(std::ios_base::dec);
    if (r.low == r.high)
        stream << r.high;
    else
        stream << "<" << r.low << ".." << r.high << ">";
    stream.flags(fl);
    return stream;
}


class CRangeList {
    using rangeList_t = std::vector<CRange>;
public:
    using iterator = rangeList_t::iterator;
    using const_iterator = rangeList_t::const_iterator;

    // constructor
    CRangeList() {};

    CRangeList(std::initializer_list<CRange> list) {
        for (auto range: list) {
            (*this) += range;
        }
    }

    iterator begin() { return list.begin(); }

    iterator end() { return list.end(); }

    const_iterator begin() const { return list.begin(); }

    const_iterator end() const { return list.end(); }

    const_iterator cbegin() const { return list.cbegin(); }

    const_iterator cend() const { return list.cend(); }

    // += range / range list
    CRangeList &operator+=(const CRange &range);

    CRangeList &operator+=(const CRangeList &list);

    // -= range / range list
    CRangeList &operator-=(const CRange &range);

    CRangeList &operator-=(const CRangeList &list);

    // operator ==
    friend bool operator==(const CRangeList &l1, const CRangeList &l2);

    friend bool operator!=(const CRangeList &l1, const CRangeList &l2);

    friend std::ostream &operator<<(std::ostream &stream, const CRangeList &l);

    // = range / range list
    CRangeList &operator=(const CRange &range);
    // operator <<

    bool equals(const CRangeList &l1) const;

    // Includes long long / range
    bool Includes(const long long n) const;

    bool Includes(const CRange &n) const;

private:
    rangeList_t list;

    std::vector<CRange>::iterator getMinRange(const CRange &range, iterator it, bool minusOne) const;
};

CRangeList &CRangeList::operator+=(const CRange &range) {
    CRange newRange = range;

    // vrátí první vyšší/roven
    iterator pos = lower_bound(list.begin(), list.end(), range);

    // zjistit ifem jestli range není v pos vrchem
    bool startPassed = false;
    if (pos != list.end()) {
        long long posLow = pos->low == LLONG_MIN ? pos->low : pos->low - 1;
        if (range.high <= pos->high && range.high >= posLow) {
            newRange.high = pos->high;
            startPassed = true;
        }
    }

    auto minPos = getMinRange(range, pos, true);

    // zjistit ifem jestli je spodem v minIt


    if (minPos != list.end()) {
        long long rangeLow = range.low == LLONG_MIN ? range.low : range.low - 1;
        if (rangeLow <= minPos->high && range.low >= minPos->low) {
            newRange.low = minPos->low;
        }
    }


    if (pos - minPos > 0) {
        *
                minPos = newRange;
        if (startPassed)
            this->list.erase(++minPos, ++pos);
        else
            this->list.erase(++minPos, pos);
    } else if (startPassed)
        *
                pos = newRange;
    else
        this->list.
                insert(minPos, newRange
        );
    return *this;
}

CRangeList &CRangeList::operator+=(const CRangeList &newList) {
    if (this != &newList) {
        for (const auto range : newList) {
            *this += range;
        }
    }
    return *this;
}

CRangeList &CRangeList::operator-=(const CRange &range) {

    iterator pos = lower_bound(list.begin(), list.end(), range);

    // zjistit ifem jestli range není v pos vrchem
    bool delLast = false;
    if (pos != list.end()) {
        if (range.high == pos->high && range.low == pos->low) {
            list.erase(pos);
            return *this;
        } else if (range.high == pos->high && range.low > pos->low) {
            pos->high = range.low - 1;
            return *this;
        } else if (range.low == pos->low) {
            pos->low = range.high + 1;
            return *this;
        } else if (range.high < pos->high && range.low > pos->low) {
            //rozdělení na dva intervaly
            long long tmpH = pos->high;
            pos->high = range.low - 1;
            list.insert(++pos, CRange(range.high + 1, tmpH));
            return *this;
        } else if (range.high < pos->high && range.high >= pos->low && range.low < pos->low) {
            pos->low = range.high + 1;
        } else if (range.high == pos->high)
            delLast = true;
    }

    auto minPos = getMinRange(range, pos, false);

    // zjistit ifem jestli je spodem v minIt
    if (minPos != list.end()) {
        if (range.low == minPos->low) {
            if (delLast)
                list.erase(minPos, ++pos);
            else
                list.erase(minPos, pos);
            return *this;
        } else if (range.low > minPos->low && range.low <= minPos->high) {
            minPos->high = range.low - 1;
            ++minPos;
        }
        if (delLast) ++pos;
        if (pos - minPos >= 1) list.erase(minPos, pos);
    }

    return *this;
}

CRangeList &CRangeList::operator-=(const CRangeList &newList) {
    for (const auto range : newList) {
        *this -= range;
    }

    return *this;
}

CRangeList &CRangeList::operator=(const CRange &range) {
    if (list.size() > 0)
        list.clear();

    list.push_back(range);
    return *this;
}

CRangeList::iterator CRangeList::getMinRange(const CRange &range, CRangeList::iterator iter, bool minusOne) const {
    iterator it = iter;
    iterator last = it;

    while (it != list.begin()) {
        --it;
        long long rangeLow = range.low == LLONG_MIN || !minusOne ? range.low : range.low - 1;
        if (rangeLow <= it->high && range.low >= it->low) {

            last = it;
            break;
        } else if (range.low <= it->low)
            last = it;
        else if (it->high < range.low) // už je pod intervalem range
            break;
    }

    return last;
}

bool CRangeList::Includes(const long long n) const {
    const_iterator pos = lower_bound(list.begin(), list.end(), CRange(n, n));

    return pos != list.end() && n <= pos->high && n >= pos->low;
}

bool CRangeList::Includes(const CRange &range) const {
    const_iterator pos = lower_bound(list.begin(), list.end(), range);

    if (pos == list.end() || range.high < pos->low)
        return false;

    if (range.high <= pos->high && range.low >= pos->low)   //celý range je v intervalue
        return true;

    // vrchní část range je v intervalue
    long long lastLow = pos->low;
    while (pos != list.begin()) {
        --pos;

        long long posHigh = pos->high == LLONG_MAX ? pos->high : pos->high + 1;
        if (range.low < pos->low || lastLow != posHigh)
            return false;
        else if (range.low <= pos->high && range.low >= pos->low)
            return true;

    }

    //kontrola jestli range nemá nižší hodnotu než všechny moje intervaly
    return range.low > pos->low;
}

bool CRangeList::equals(const CRangeList &l2) const {
    auto it1 = list.begin();
    auto it2 = l2.begin();

    while (it1 != list.end() && it2 != l2.end()) {

        if (it1->low != it2->low || it1->high != it2->high)
            return false;
        ++it1;
        ++it2;
    }

    return it1 == list.end() && it2 == l2.end();
}


std::ostream &operator<<(std::ostream &stream, const CRangeList &l) {
    stream << "{";

    string delim = "";
    for (auto const &range: l) {
        stream << delim << range;
        delim = ",";
    }
    stream << "}";
    return stream;
}

CRangeList operator+(const CRange &a, const CRange &b) {
    CRangeList l;
    l += a;
    l += b;
    return l;
}

CRangeList operator+(const CRangeList &list, const CRange &b) {
    CRangeList l(list);
    l += b;
    return l;
}

CRangeList operator-(const CRange &a, const CRange &b) {
    CRangeList l;
    l += a;
    l -= b;
    return l;
}

CRangeList operator-(const CRangeList &list, const CRange &b) {
    CRangeList l(list);
    l -= b;
    return l;
}

bool operator==(const CRangeList &l1, const CRangeList &l2) {
    return l1.equals(l2);
}

bool operator!=(const CRangeList &l1, const CRangeList &l2) {
    return !l1.equals(l2);
}

#ifndef __PROGTEST__

string toString(const CRangeList &x) {
    ostringstream oss;
    oss << x;
    return oss.str();
}


int main(void) {

    CRangeList a, b;

    //custom
    a += CRange(0, 10) + CRange(14, 18) + CRange(20, 60);
    a -= CRange(8, 59);

    a = CRange(10, 15);
    string aaa = toString(a);
    assert (toString(a) == "{<10..15>}");
    a += CRange(0, 2);
    assert (toString(a) == "{<0..2>,<10..15>}");
    a += CRange(8, 9);
    assert (toString(a) == "{<0..2>,<8..15>}");
    a += CRange(11, 17);
    assert (toString(a) == "{<0..2>,<8..17>}");
    a += CRange(4, 5);
    assert (toString(a) == "{<0..2>,<4..5>,<8..17>}");
    a += CRange(2, 3);
    assert (toString(a) == "{<0..5>,<8..17>}");
    a += CRange(-3, 20);
    assert (toString(a) == "{<-3..20>}");

    a = CRange(10, 15) + CRange(20, 40) + CRange(45, 90) + CRange(150, 400);
    a -= CRange(90, 100);
    assert (toString(a) == "{<10..15>,<20..40>,<45..89>,<150..400>}");
    a -= CRange(400, 500);
    assert (toString(a) == "{<10..15>,<20..40>,<45..89>,<150..399>}");
    a -= CRange(25, 26);
    assert (toString(a) == "{<10..15>,<20..24>,<27..40>,<45..89>,<150..399>}");
    a -= CRange(14, 44);
    assert (toString(a) == "{<10..13>,<45..89>,<150..399>}");
    // orig

    assert (sizeof(CRange) <= 2 * sizeof(long long));
    a = CRange(5, 10);
    a += CRange(25, 100);
    string s = toString(a);
    assert (toString(a) == "{<5..10>,<25..100>}");
    a += CRange(-5, 0);
    a += CRange(8, 50);
    assert (toString(a) == "{<-5..0>,<5..100>}");
    a += CRange(101, 105) + CRange(120, 150) + CRange(160, 180) + CRange(190, 210);
    assert (toString(a) == "{<-5..0>,<5..105>,<120..150>,<160..180>,<190..210>}");
    a += CRange(106, 119) + CRange(152, 158);
    assert (toString(a) == "{<-5..0>,<5..150>,<152..158>,<160..180>,<190..210>}");
    a += CRange(-3, 170);
    a += CRange(-30, 1000);
    assert (toString(a) == "{<-30..1000>}");
    b = CRange(-500, -300) + CRange(2000, 3000) + CRange(700, 1001);
    a += b;
    assert (toString(a) == "{<-500..-300>,<-30..1001>,<2000..3000>}");
    a -= CRange(-400, -400);
    assert (toString(a) == "{<-500..-401>,<-399..-300>,<-30..1001>,<2000..3000>}");
    a -= CRange(10, 20) + CRange(900, 2500) + CRange(30, 40) + CRange(10000, 20000);
    assert (toString(a) == "{<-500..-401>,<-399..-300>,<-30..9>,<21..29>,<41..899>,<2501..3000>}");
    try {
        a += CRange(15, 18) + CRange(10, 0) + CRange(35, 38);
        assert ("Exception not thrown" == NULL);
    }
    catch (const InvalidRangeException &e) {
    }
    catch (...) {
        assert ("Invalid exception thrown" == NULL);
    }
    assert (toString(a) == "{<-500..-401>,<-399..-300>,<-30..9>,<21..29>,<41..899>,<2501..3000>}");
    b = a;
    assert (a == b);
    assert (!(a != b));
    b += CRange(2600, 2700);
    assert (toString(b) == "{<-500..-401>,<-399..-300>,<-30..9>,<21..29>,<41..899>,<2501..3000>}");
    assert (a == b);
    assert (!(a != b));
    b += CRange(15, 15);
    assert (toString(b) == "{<-500..-401>,<-399..-300>,<-30..9>,15,<21..29>,<41..899>,<2501..3000>}");
    assert (!(a == b));
    assert (a != b);
    assert (b.Includes(15));
    assert (b.Includes(2900));
    assert (b.Includes(CRange(15, 15)));
    assert (b.Includes(CRange(-350, -350)));
    assert (b.Includes(CRange(100, 200)));
    assert (!b.Includes(CRange(800, 900)));
    assert (!b.Includes(CRange(-1000, -450)));
    assert (!b.Includes(CRange(0, 500)));
    a += CRange(-10000, 10000) + CRange(10000000, 1000000000);
    assert (toString(a) == "{<-10000..10000>,<10000000..1000000000>}");
    b += a;
    assert (toString(b) == "{<-10000..10000>,<10000000..1000000000>}");
    b -= a;
    assert (toString(b) == "{}");
    b += CRange(0, 100) + CRange(200, 300) - CRange(150, 250) + CRange(160, 180) - CRange(170, 170);
    assert (toString(b) == "{<0..100>,<160..169>,<171..180>,<251..300>}");
    b -= CRange(10, 90) - CRange(20, 30) - CRange(40, 50) - CRange(60, 90) + CRange(70, 80);
    assert (toString(b) == "{<0..9>,<20..30>,<40..50>,<60..69>,<81..100>,<160..169>,<171..180>,<251..300>}");
#ifdef EXTENDED_SYNTAX
    CRangeList x{{5,   20},
                 {150, 200},
                 {-9,  12},
                 {48,  93}};
    assert (toString(x) == "{<-9..20>,<48..93>,<150..200>}");
    ostringstream oss;
    oss << setfill('=') << hex << left;
    for (const auto &v : x + CRange(-100, -100))
        oss << v << endl;
    oss << setw(10) << 1024;
    string sss = oss.str();
    assert (oss.str() == "-100\n<-9..20>\n<48..93>\n<150..200>\n400=======");
#endif /* EXTENDED_SYNTAX */
    return 0;
}

#endif /* __PROGTEST__ */
