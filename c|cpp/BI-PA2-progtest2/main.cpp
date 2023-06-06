#ifndef __PROGTEST__

#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <cctype>
#include <cmath>
#include <cassert>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <list>
#include <algorithm>
#include <functional>
#include <memory>

using namespace std;
#endif /* __PROGTEST__ */

bool iequals(const string &a, const string &b) {
    unsigned int sz = a.size();
    if (b.size() != sz)
        return false;
    for (unsigned int i = 0; i < sz; ++i)
        if (tolower(a[i]) != tolower(b[i]))
            return false;
    return true;
}

class Land {
public:
    Land(const string &city, const string &addr,
         const string &region,
         unsigned int id);

    string city;
    string addr;
    string region;
    string owner;
    unsigned int id;
};

Land::Land(const string &city, const string &addr, const string &region, unsigned int id) : city(city), addr(addr),
                                                                                            region(region), id(id),
                                                                                            owner("") {};

class CIterator {
public:
    CIterator(vector<Land *> vector);

    bool AtEnd(void) const;

    void Next(void);

    string City(void) const;

    string Addr(void) const;

    string Region(void) const;

    unsigned ID(void) const;

    string Owner(void) const;

private:
    vector<Land *> v;
    int index = 0;
};

CIterator::CIterator(vector<Land *> vector) : v(vector) {}

bool CIterator::AtEnd() const {
    return v.size() == index;
}

void CIterator::Next() {
    ++index;
}

string CIterator::City() const {
    return v[index]->city;
}

string CIterator::Addr() const {
    return v[index]->addr;
}

string CIterator::Region() const {
    return v[index]->region;
}

unsigned CIterator::ID() const {
    return v[index]->id;
}

string CIterator::Owner() const {
    return v[index]->owner;
}

//vector<vector<Land *>*>
class CLandRegister {
public:
    CLandRegister() {
        // vByOwner = vector<vector<Land *> *>(1);
    }

    bool Add(const string &city,
             const string &addr,
             const string &region,
             unsigned int id);

    bool Del(const string &city,
             const string &addr);

    bool Del(const string &region,
             unsigned int id);

    bool GetOwner(const string &city,
                  const string &addr,
                  string &owner) const;

    bool GetOwner(const string &region,
                  unsigned int id,
                  string &owner) const;

    bool NewOwner(const string &city,
                  const string &addr,
                  const string &owner);

    bool NewOwner(const string &region,
                  unsigned int id,
                  const string &owner);

    unsigned Count(const string &owner) const;

    CIterator ListByAddr(void) const;

    CIterator ListByOwner(const string &owner) const;

private:
    vector<Land *> vByCity;
    vector<Land *> vByRegion;
    vector<vector<Land *>> vByOwner;
};

bool getIndexInCity(unsigned long vlevo, unsigned long vpravo, vector<Land *> v, const string city, const string addr,
                    unsigned long &endIndex) {
    unsigned long stred;
    while (vlevo <= vpravo) {
        stred = (vpravo + vlevo) / 2;
        if (v[stred]->city.compare(city) == 0 && v[stred]->addr.compare(addr) == 0) {
            endIndex = stred;
            return true;
        }
        int first = v[stred]->city.compare(city) > 0;
        int second = v[stred]->city.compare(city) == 0 && v[stred]->addr.compare(addr) > 0;
        if (v[stred]->city.compare(city) > 0 ||
            (v[stred]->city.compare(city) == 0 && v[stred]->addr.compare(addr) > 0)) {
            vpravo = stred - 1;
            if (stred == 0) {
                endIndex = stred;
                return false;
            }
        } else
            vlevo = stred + 1;
    }
    if (vpravo < vlevo) {
        endIndex = stred;
        return false;
    }
    endIndex = vlevo;
    return false;
};

bool
getIndexInAddr(unsigned long vlevo, unsigned long vpravo, vector<Land *> v, const string region, const unsigned int id,
               unsigned long &endIndex) {
    unsigned long stred;
    while (vlevo <= vpravo) {
        stred = (vpravo + vlevo) / 2;
        if (v[stred]->region.compare(region) == 0 && v[stred]->id == id) {
            endIndex = stred;
            return true;
        }
        int jed = v[stred]->region.compare(region);
        bool dva = (v[stred]->region.compare(region) == 0 && v[stred]->id > id);

        if (v[stred]->region.compare(region) > 0 || (v[stred]->region.compare(region) == 0 && v[stred]->id > id)) {
            vpravo = stred - 1;
            if (stred == 0) {
                endIndex = stred;
                return false;
            }
        } else {
            if (stred == 0 && vpravo == stred) {
                endIndex = stred + 1;
                return false;
            }

            vlevo = stred + 1;
        }
    }
    if (vpravo < vlevo) {
        endIndex = stred;
        return false;
    }
    endIndex = vlevo;
    return false;
}

bool CLandRegister::Add(const string &city, const string &addr, const string &region, unsigned int id) {
    Land *newLand = new Land(city, addr, region, id);

    // najít kam umístit prvek a insertem ho tam dát
    unsigned long citySize = vByCity.size();
    unsigned long addrSize = vByRegion.size();
    if (citySize > 0) {
        unsigned long indexCity;
        unsigned long indexAddr;
        if (!getIndexInCity(0, citySize - 1, vByCity, newLand->city, newLand->addr, indexCity)
            && !getIndexInAddr(0, addrSize - 1, vByRegion, newLand->region, newLand->id, indexAddr)) {


            vByOwner[0].push_back(newLand);
            Land landink = *(vByOwner[0][1]);
            //  a = vvv.size();

            vByCity.insert(vByCity.begin() + indexCity, newLand);
            vByRegion.insert(vByRegion.begin() + indexAddr, newLand);

            // (*vByOwner[0]).push_back(newLand);

        } else return false; //již tento prvek je uložen
    } else {
        vector<Land *> newV;
        newV.push_back(newLand);

        vByOwner.push_back(newV);
        vByCity.push_back(newLand);
        vByRegion.push_back(newLand);

        // int a = vByOwner.size();


        //vByOwner.push_back(newV);
    }

    return true;
}

bool
getIndexInOwner(unsigned long vpravo, vector<vector<Land *>> v, const string owner,
                unsigned long &endIndex, bool insensitive = false) {
    unsigned long vlevo = 0;
    //unsigned long stred = 1; // když přijde vpravo 0 -> nejsou ještě žádní vlsatnící -> přidat nový na 1 index
    unsigned long stred;
    while (vlevo <= vpravo) {
        stred = (vpravo + vlevo) / 2;
        if (v[stred][0]->owner.compare(owner) == 0 || (insensitive && iequals(v[stred][0]->owner, owner))) {
            endIndex = stred;
            return true;
        }
        if (v[stred][0]->owner.compare(owner) > 0) {
            vpravo = stred - 1;
            if (stred == 0) {
                endIndex = stred;
                return false;
            }
        } else
            vlevo = stred + 1;
    }
    if (vpravo < vlevo) {
        endIndex = stred;
        return false;
    }
    endIndex = vlevo;
    return false;
}

bool CLandRegister::Del(const string &region, unsigned int id) {};

bool CLandRegister::Del(const string &city, const string &addr) {};

bool CLandRegister::NewOwner(const string &region, unsigned int id, const string &owner) {
    unsigned long indexInAddr;
    if (getIndexInAddr(0, vByRegion.size() - 1, vByRegion, region, id, indexInAddr)) {
        Land *land = vByRegion[indexInAddr];
        if (land->owner.compare(owner) == 0) return false;

        //odebrání pozemku z vlastníku
        unsigned long indexOfOrigOwner;
        if (getIndexInOwner(vByOwner.size() - 1, vByOwner, land->owner, indexOfOrigOwner, true)) {
            const vector<Land *> ownerVec = vByOwner[indexOfOrigOwner];

            const auto iter = ownerVec.begin();
            const auto iter2 = vByOwner.begin();


//            ownerVec.erase( 1);

            for (unsigned long i = 0; i < ownerVec.size(); ++i) {
                Land *tmp = ownerVec[i];
                if (tmp->id == id && tmp->region.compare(region) == 0) {
                    cout << tmp->id;
                    // ownerVec.erase(ownerVec.begin());
                }
            }
        }


        unsigned long indexInOwner;
        if (getIndexInOwner(vByOwner.size() - 1, vByOwner, owner, indexInOwner, true)) {
            vByOwner[indexInOwner].push_back(vByRegion[indexInAddr]);
        } else {
            vector<Land *> newV;
            newV.push_back(vByRegion[indexInAddr]);

            vByOwner.insert(vByOwner.begin() + indexInOwner, newV);
        }

        vByRegion[indexInAddr]->owner = owner;
        return true;
    }
    return false;
};

bool CLandRegister::NewOwner(const string &city, const string &addr, const string &owner) {
    unsigned long indexInCity;
    if (getIndexInCity(0, vByCity.size() - 1, vByCity, city, addr, indexInCity)) {
        Land *land = vByCity[indexInCity];
        if (land->owner.compare(owner) == 0) return false;

        unsigned long indexOfOrigOwner;
        if (getIndexInOwner(vByOwner.size() - 1, vByOwner, land->owner, indexOfOrigOwner, true)) {
            const vector<Land *> ownerVec = vByOwner[indexOfOrigOwner];

            const auto iter = ownerVec.begin() ;
            const std::vector<vector<Land*>>::iterator iter2 = vByOwner.begin();


//            ownerVec.erase( 1);

            for (unsigned long i = 0; i < ownerVec.size(); ++i) {
                Land *tmp = ownerVec[i];
                if (tmp->addr.compare(addr) == 0 && tmp->city.compare(city) == 0) {
                    cout << tmp->id;
                    //ownerVec.erase(ownerVec.begin());
                }
            }
        }

        unsigned long indexInOwner;
        if (getIndexInOwner(vByOwner.size() - 1, vByOwner, owner, indexInOwner, true)) {
            vByOwner[indexInOwner].push_back(vByCity[indexInCity]);
        } else {
            vector<Land *> newV;
            newV.push_back(vByCity[indexInCity]);

            vByOwner.insert(vByOwner.begin() + indexInOwner, newV);
        }

        vByCity[indexInCity]->owner = owner;
        return true;

    } else return false;
}

bool CLandRegister::GetOwner(const string &region, unsigned int id, string &owner) const {
    unsigned long index;
    if (getIndexInAddr(0, vByRegion.size() - 1, vByRegion, region, id, index)) {
        owner = vByRegion[index]->owner;
        return true;
    }
    return false;
}

bool CLandRegister::GetOwner(const string &city, const string &addr, string &owner) const {
    unsigned long index;
    if (getIndexInCity(0, vByCity.size() - 1, vByCity, city, addr, index)) {
        owner = vByCity[index]->owner;
        return true;
    }
    return false;

}

unsigned CLandRegister::Count(const string &owner) const {
    if (string("").compare(owner) == 0) {
        unsigned counter = 0;
        for (int i = 0; i < vByOwner[0].size(); ++i) {
            if (vByOwner[0][i]->owner.compare("") == 0)
                ++counter;
        }
        return counter;
    }

    unsigned long index;
    if (getIndexInOwner(vByOwner.size() - 1, vByOwner, owner, index, true)) {
        return vByOwner[index].size();
    }

    return 0;
}

CIterator CLandRegister::ListByAddr() const {

    return CIterator(vByCity);
}

CIterator CLandRegister::ListByOwner(const string &owner) const {
    if (string("").compare(owner) == 0) {
        vector<Land *> v;
        for (int i = 0; i < vByOwner[0].size(); ++i) {
            if (vByOwner[0][i]->owner.compare("") == 0)
                v.push_back(vByOwner[0][i]);
        }
        return CIterator(v);
    }

    unsigned long index;
    if (getIndexInOwner(vByOwner.size() - 1, vByOwner, owner, index, true)) {
        return CIterator(vByOwner[index]);
    } else {
        vector<Land *> empty;
        return CIterator(empty);
    }
}

#ifndef __PROGTEST__

static void test0(void) {
    CLandRegister x;
    string owner;

    assert(iequals(string("CVUT"), string("cvut")));

    assert(x.Add("Prague", "Thakurova", "Dejvice", 12345));
    assert(x.Add("Prague", "Evropska", "Vokovice", 12345));
    assert(x.Add("Prague", "Technicka", "Dejvice", 9873));
    assert(x.Add("Plzen", "Evropska", "Plzen mesto", 78901));
    assert(x.Add("Liberec", "Evropska", "Librec", 4552));
    CIterator i0 = x.ListByAddr();
    assert(!i0.AtEnd() && i0.City() == "Liberec" && i0.Addr() == "Evropska" && i0.Region() == "Librec" &&
           i0.ID() == 4552 && i0.Owner() == "");
    i0.Next();
    assert(!i0.AtEnd() && i0.City() == "Plzen" && i0.Addr() == "Evropska" && i0.Region() == "Plzen mesto" &&
           i0.ID() == 78901 && i0.Owner() == "");
    i0.Next();
    assert(!i0.AtEnd() && i0.City() == "Prague" && i0.Addr() == "Evropska" && i0.Region() == "Vokovice" &&
           i0.ID() == 12345 && i0.Owner() == "");
    i0.Next();
    assert(!i0.AtEnd() && i0.City() == "Prague" && i0.Addr() == "Technicka" && i0.Region() == "Dejvice" &&
           i0.ID() == 9873 && i0.Owner() == "");
    i0.Next();
    assert(!i0.AtEnd() && i0.City() == "Prague" && i0.Addr() == "Thakurova" && i0.Region() == "Dejvice" &&
           i0.ID() == 12345 && i0.Owner() == "");
    i0.Next();
    assert(i0.AtEnd());

    assert(x.Count("") == 5);
    CIterator i1 = x.ListByOwner("");
    assert(!i1.AtEnd() && i1.City() == "Prague" && i1.Addr() == "Thakurova" && i1.Region() == "Dejvice" &&
           i1.ID() == 12345 && i1.Owner() == "");
    i1.Next();
    assert(!i1.AtEnd() && i1.City() == "Prague" && i1.Addr() == "Evropska" && i1.Region() == "Vokovice" &&
           i1.ID() == 12345 && i1.Owner() == "");
    i1.Next();
    assert(!i1.AtEnd() && i1.City() == "Prague" && i1.Addr() == "Technicka" && i1.Region() == "Dejvice" &&
           i1.ID() == 9873 && i1.Owner() == "");
    i1.Next();
    assert(!i1.AtEnd() && i1.City() == "Plzen" && i1.Addr() == "Evropska" && i1.Region() == "Plzen mesto" &&
           i1.ID() == 78901 && i1.Owner() == "");
    i1.Next();
    assert(!i1.AtEnd() && i1.City() == "Liberec" && i1.Addr() == "Evropska" && i1.Region() == "Librec" &&
           i1.ID() == 4552 && i1.Owner() == "");
    i1.Next();
    assert(i1.AtEnd());

    assert(x.Count("CVUT") == 0);
    CIterator i2 = x.ListByOwner("CVUT");
    assert(i2.AtEnd());

    assert(x.NewOwner("Prague", "Thakurova", "CVUT"));
    assert(x.NewOwner("Dejvice", 9873, "CVUT"));
    assert(x.NewOwner("Plzen", "Evropska", "Anton Hrabis"));
    assert(x.NewOwner("Librec", 4552, "Cvut"));
    assert(x.GetOwner("Prague", "Thakurova", owner) && owner == "CVUT");
    assert(x.GetOwner("Dejvice", 12345, owner) && owner == "CVUT");
    assert(x.GetOwner("Prague", "Evropska", owner) && owner == "");
    assert(x.GetOwner("Vokovice", 12345, owner) && owner == "");
    assert(x.GetOwner("Prague", "Technicka", owner) && owner == "CVUT");
    assert(x.GetOwner("Dejvice", 9873, owner) && owner == "CVUT");
    assert(x.GetOwner("Plzen", "Evropska", owner) && owner == "Anton Hrabis");
    assert(x.GetOwner("Plzen mesto", 78901, owner) && owner == "Anton Hrabis");
    assert(x.GetOwner("Liberec", "Evropska", owner) && owner == "Cvut");
    assert(x.GetOwner("Librec", 4552, owner) && owner == "Cvut");
    CIterator i3 = x.ListByAddr();
    assert(!i3.AtEnd() && i3.City() == "Liberec" && i3.Addr() == "Evropska" && i3.Region() == "Librec" &&
           i3.ID() == 4552 && i3.Owner() == "Cvut");
    i3.Next();
    assert(!i3.AtEnd() && i3.City() == "Plzen" && i3.Addr() == "Evropska" && i3.Region() == "Plzen mesto" &&
           i3.ID() == 78901 && i3.Owner() == "Anton Hrabis");
    i3.Next();
    assert(!i3.AtEnd() && i3.City() == "Prague" && i3.Addr() == "Evropska" && i3.Region() == "Vokovice" &&
           i3.ID() == 12345 && i3.Owner() == "");
    i3.Next();
    assert(!i3.AtEnd() && i3.City() == "Prague" && i3.Addr() == "Technicka" && i3.Region() == "Dejvice" &&
           i3.ID() == 9873 && i3.Owner() == "CVUT");
    i3.Next();
    assert(!i3.AtEnd() && i3.City() == "Prague" && i3.Addr() == "Thakurova" && i3.Region() == "Dejvice" &&
           i3.ID() == 12345 && i3.Owner() == "CVUT");
    i3.Next();
    assert(i3.AtEnd());

    assert(x.Count("cvut") == 3);
    CIterator i4 = x.ListByOwner("cVuT");
    assert(!i4.AtEnd() && i4.City() == "Prague" && i4.Addr() == "Thakurova" && i4.Region() == "Dejvice" &&
           i4.ID() == 12345 && i4.Owner() == "CVUT");
    i4.Next();
    assert(!i4.AtEnd() && i4.City() == "Prague" && i4.Addr() == "Technicka" && i4.Region() == "Dejvice" &&
           i4.ID() == 9873 && i4.Owner() == "CVUT");
    i4.Next();
    assert(!i4.AtEnd() && i4.City() == "Liberec" && i4.Addr() == "Evropska" && i4.Region() == "Librec" &&
           i4.ID() == 4552 && i4.Owner() == "Cvut");
    i4.Next();
    assert(i4.AtEnd());

    assert(x.NewOwner("Plzen mesto", 78901, "CVut"));
    assert(x.Count("CVUT") == 4);
    CIterator i5 = x.ListByOwner("CVUT");
    assert(!i5.AtEnd() && i5.City() == "Prague" && i5.Addr() == "Thakurova" && i5.Region() == "Dejvice" &&
           i5.ID() == 12345 && i5.Owner() == "CVUT");
    i5.Next();
    assert(!i5.AtEnd() && i5.City() == "Prague" && i5.Addr() == "Technicka" && i5.Region() == "Dejvice" &&
           i5.ID() == 9873 && i5.Owner() == "CVUT");
    i5.Next();
    assert(!i5.AtEnd() && i5.City() == "Liberec" && i5.Addr() == "Evropska" && i5.Region() == "Librec" &&
           i5.ID() == 4552 && i5.Owner() == "Cvut");
    i5.Next();
    assert(!i5.AtEnd() && i5.City() == "Plzen" && i5.Addr() == "Evropska" && i5.Region() == "Plzen mesto" &&
           i5.ID() == 78901 && i5.Owner() == "CVut");
    i5.Next();
    assert(i5.AtEnd());

    assert(x.Del("Liberec", "Evropska"));
    assert(x.Del("Plzen mesto", 78901));
    assert(x.Count("cvut") == 2);
    CIterator i6 = x.ListByOwner("cVuT");
    assert(!i6.AtEnd() && i6.City() == "Prague" && i6.Addr() == "Thakurova" && i6.Region() == "Dejvice" &&
           i6.ID() == 12345 && i6.Owner() == "CVUT");
    i6.Next();
    assert(!i6.AtEnd() && i6.City() == "Prague" && i6.Addr() == "Technicka" && i6.Region() == "Dejvice" &&
           i6.ID() == 9873 && i6.Owner() == "CVUT");
    i6.Next();
    assert(i6.AtEnd());

    assert(x.Add("Liberec", "Evropska", "Librec", 4552));
}

static void test1(void) {
    CLandRegister x;
    string owner;

    assert(x.Add("Prague", "Thakurova", "Dejvice", 12345));
    assert(x.Add("Prague", "Evropska", "Vokovice", 12345));
    assert(x.Add("Prague", "Technicka", "Dejvice", 9873));
    assert(!x.Add("Prague", "Technicka", "Hradcany", 7344));
    assert(!x.Add("Brno", "Bozetechova", "Dejvice", 9873));
    assert(!x.GetOwner("Prague", "THAKUROVA", owner));
    assert(!x.GetOwner("Hradcany", 7343, owner));
    CIterator i0 = x.ListByAddr();
    assert(!i0.AtEnd() && i0.City() == "Prague" && i0.Addr() == "Evropska" && i0.Region() == "Vokovice" &&
           i0.ID() == 12345 && i0.Owner() == "");
    i0.Next();
    assert(!i0.AtEnd() && i0.City() == "Prague" && i0.Addr() == "Technicka" && i0.Region() == "Dejvice" &&
           i0.ID() == 9873 && i0.Owner() == "");
    i0.Next();
    assert(!i0.AtEnd() && i0.City() == "Prague" && i0.Addr() == "Thakurova" && i0.Region() == "Dejvice" &&
           i0.ID() == 12345 && i0.Owner() == "");
    i0.Next();
    assert(i0.AtEnd());

    assert(x.NewOwner("Prague", "Thakurova", "CVUT"));
    assert(!x.NewOwner("Prague", "technicka", "CVUT"));
    assert(!x.NewOwner("prague", "Technicka", "CVUT"));
    assert(!x.NewOwner("dejvice", 9873, "CVUT"));
    assert(!x.NewOwner("Dejvice", 9973, "CVUT"));
    assert(!x.NewOwner("Dejvice", 12345, "CVUT"));
    assert(x.Count("CVUT") == 1);
    CIterator i1 = x.ListByOwner("CVUT");
    assert(!i1.AtEnd() && i1.City() == "Prague" && i1.Addr() == "Thakurova" && i1.Region() == "Dejvice" &&
           i1.ID() == 12345 && i1.Owner() == "CVUT");
    i1.Next();
    assert(i1.AtEnd());

    assert(!x.Del("Brno", "Technicka"));
    assert(!x.Del("Karlin", 9873));
    assert(x.Del("Prague", "Technicka"));
    assert(!x.Del("Prague", "Technicka"));
    assert(!x.Del("Dejvice", 9873));
}

int main(void) {
    test0();
    test1();
    return 0;
}

#endif /* __PROGTEST__ */
