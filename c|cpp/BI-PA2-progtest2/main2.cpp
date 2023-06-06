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

int iequals(const string &a, const string &b) {
    return strcasecmp(a.c_str(), b.c_str());
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

Land::Land(const string &city, const string &addr, const string &region, unsigned int i) : city(city), addr(addr),
                                                                                           region(region),
                                                                                           owner(""), id(i) {}


struct Owner {
    string name;
    vector<Land *> v;

    Owner(string n) : name(n), v() {};
};

class CIterator {
public:
    CIterator(const vector<Land *> &vector);

    bool AtEnd(void) const;

    void Next(void);

    string City(void) const;

    string Addr(void) const;

    string Region(void) const;

    unsigned ID(void) const;

    string Owner(void) const;

private:
    vector<Land *> v;
    unsigned long index = 0;
};

CIterator::CIterator(const vector<Land *> &vector) {
    v = vector;
}

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

class CLandRegister {
public:
    ~CLandRegister();

    CLandRegister();

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
    vector<Owner *> vOwners;

    void addToOwner(Land *land, const string &owner);

    void removeFromOwners(Land *land);

};

void
binary_searchAddr(const vector<Land *> &v, const string &region, const unsigned int id, long long imin, long long imax,
                  unsigned long &endIndex, bool &found) {
    if (imax < imin) {
        //  Insert key as the next element after imax
        endIndex = imax + 1;
        found = false;
        return;
    } else {
        // calculate midpoint to cut set in half
        long long imid = imin + ((imax - imin ) / 2);

        // three-way comparison
        int diffRegion = v[imid]->region.compare(region);
        if (diffRegion > 0 || (diffRegion == 0 && v[imid]->id > id))
            // key is in lower subset
            return binary_searchAddr(v, region, id, imin, imid - 1, endIndex, found);
        else if (diffRegion < 0 || (diffRegion == 0 && v[imid]->id < id))
            // key is in upper subset
            return binary_searchAddr(v, region, id, imid + 1, imax, endIndex, found);
        else {
            // key has been found so insert it after imax
            endIndex = imid;
            found = true;
            return;
        }
    }
}

bool binaryByAddr(const vector<Land *> &v, const string &region, const unsigned int id, unsigned long &endIndex) {
    bool found = false;
    endIndex = 0;
    if (v.size() > 0) {
        binary_searchAddr(v, region, id, 0, v.size() - 1, endIndex, found);
    }

    return found;
}


void binary_searchCity(const vector<Land *> &v, const string &city, const string addr, long long imin, long long imax,
                       unsigned long &endIndex, bool &found) {
    if (imax < imin) {
        // Insert key as the next element after imax
        endIndex = imax + 1;
        found = false;
        return;
    } else {
        // calculate midpoint to cut set in half
        long long imid = imin + ((imax - imin ) / 2);

        // three-way comparison
        int diffCity = v[imid]->city.compare(city);
        int diffAddr = v[imid]->addr.compare(addr);
        if (diffCity > 0 ||
            (diffCity == 0 && diffAddr > 0))
            // key is in lower subset
            return binary_searchCity(v, city, addr, imin, imid - 1, endIndex, found);
        else if (diffCity < 0 ||
                 (diffCity == 0 && diffAddr < 0))
            // key is in upper subset
            return binary_searchCity(v, city, addr, imid + 1, imax, endIndex, found);
        else {
            // key has been found so insert it after imax
            endIndex = imid;
            found = true;
            return;
        }
    }
}

bool binaryByCity(const vector<Land *> &v, const string &city, const string &addr, unsigned long &endIndex) {
    bool found = false;
    endIndex = 0;
    if (v.size() > 0) {

        binary_searchCity(v, city, addr, 0, v.size() - 1, endIndex, found);
    }

    return found;
}

void binary_searchOwner(const vector<Owner *> &v, const string &owner, long long imin, long long imax,
                        unsigned long &endIndex, bool &found) {
    if (imax < imin) {
        // Insert key as the next element after imax
        endIndex = imax + 1;
        found = false;
        return;
    } else {
        // calculate midpoint to cut set in half
        long long imid = imin + ((imax - imin ) / 2);

        int diffName = iequals(v[imid]->name, owner);
        if (diffName > 0) {
            return binary_searchOwner(v, owner, imin, imid - 1, endIndex, found);
        } else if (diffName < 0)
            // key is in upper subset
            return binary_searchOwner(v, owner, imid + 1, imax, endIndex, found);
        else {
            //   iequals(v[imid]->name, owner) == 0
            endIndex = imid;
            found = true;
            return;
        }
    }
}

bool binaryByOwner(const vector<Owner *> &v, const string &owner, unsigned long &endIndex) {
    bool found = false;
    endIndex = 0;
    if (v.size() > 0) {
        binary_searchOwner(v, owner, 0, v.size() - 1, endIndex, found);
    }

    return found;
}

CLandRegister::CLandRegister() {
    Owner *newOwner = new Owner("");
    vOwners.push_back(newOwner);
}

CLandRegister::~CLandRegister() {
    for (auto const value: vByCity) {
        delete value;
    }

    for (auto const value: vOwners) {
        delete value;
    }

}

bool CLandRegister::Add(const string &city, const string &addr, const string &region, unsigned int id) {
    Land *newLand = new Land(city, addr, region, id);

    // najít kam umístit prvek a insertem ho tam dát
    unsigned long indexCity;
    unsigned long indexAddr;
    if (!binaryByCity(vByCity, newLand->city, newLand->addr, indexCity)
        && !binaryByAddr(vByRegion, newLand->region, newLand->id, indexAddr)) {

        //addToOwner(newLand, newLand->owner);
        vOwners[0]->v.push_back(newLand);
        vByCity.insert(vByCity.begin() + indexCity, newLand);
        vByRegion.insert(vByRegion.begin() + indexAddr, newLand);
    } else {
        delete newLand;
        return false; //již tento prvek je uložen
    }



    return true;
}


bool CLandRegister::Del(const string &region, unsigned int id) {
    unsigned long index;
    if (binaryByAddr(vByRegion, region, id, index)) {
        Land *land = vByRegion[index];

        vByRegion.erase(vByRegion.begin() + index);
        binaryByCity(vByCity, land->city, land->addr, index);
        vByCity.erase(vByCity.begin() + index);

        removeFromOwners(land);

        delete land;
        return true;
    }
    return false;
}

bool CLandRegister::Del(const string &city, const string &addr) {
    unsigned long index;
    if (binaryByCity(vByCity, city, addr, index)) {
        Land *land = vByCity[index];
        vByCity.erase(vByCity.begin() + index);

        binaryByAddr(vByRegion, land->region, land->id, index);
        vByRegion.erase(vByRegion.begin() + index);

        removeFromOwners(land);

        delete land;
        return true;
    }
    return false;
}

void CLandRegister::addToOwner(Land *land, const string &owner) {
    unsigned long indexInOwner;
    if (binaryByOwner(vOwners, owner, indexInOwner)) {
        Owner *owner = vOwners[indexInOwner];
        owner->v.push_back(land);
    } else {
        Owner *newOwner = new Owner(owner);
        newOwner->v.push_back(land);
        vOwners.insert(vOwners.begin() + indexInOwner, newOwner);
    }

    land->owner = owner;
}

void CLandRegister::removeFromOwners(Land *land) {
    unsigned long indexInOwner;
    binaryByOwner(vOwners, land->owner, indexInOwner);
    Owner *owner = vOwners[indexInOwner];


    for (unsigned i = 0; i < owner->v.size(); i++) {
        Land *tmp = owner->v[i];
        if (tmp->region.compare(land->region) == 0 && tmp->id == land->id) {
            owner->v.erase(owner->v.cbegin() + i);
            break;
        }
    }

    if (owner->v.size() == 0 && !owner->name.empty()) {
        delete vOwners[indexInOwner];
        vOwners.erase(vOwners.begin() + indexInOwner);
    }
}

bool CLandRegister::NewOwner(const string &region, unsigned int id, const string &owner) {
    unsigned long indexInAddr;
    if (binaryByAddr(vByRegion, region, id, indexInAddr)) {
        Land *land = vByRegion[indexInAddr];
        if (land->owner.compare(owner) == 0) return false;

        removeFromOwners(land);

        addToOwner(land, owner);
        return true;
    }
    return false;
}

bool CLandRegister::NewOwner(const string &city, const string &addr, const string &owner) {
    unsigned long indexInCity;
    if (binaryByCity(vByCity, city, addr, indexInCity)) {
        Land *land = vByCity[indexInCity];
        if (land->owner.compare(owner) == 0) return false;

        removeFromOwners(land);

        addToOwner(land, owner);
        return true;
    }
    return false;
}

bool CLandRegister::GetOwner(const string &region, unsigned int id, string &owner) const {
    unsigned long index;
    if (binaryByAddr(vByRegion, region, id, index)) {
        owner = vByRegion[index]->owner;
        return true;
    }
    return false;
}

bool CLandRegister::GetOwner(const string &city, const string &addr, string &owner) const {
    unsigned long index;
    if (binaryByCity(vByCity, city, addr, index)) {
        owner = vByCity[index]->owner;
        return true;
    }
    return false;

}

unsigned CLandRegister::Count(const string &owner) const {
    unsigned long index;
    if (binaryByOwner(vOwners, owner, index)) {
        return vOwners[index]->v.size();
    }

    return 0;
}

CIterator CLandRegister::ListByAddr() const {

    return CIterator(vByCity);
}

CIterator CLandRegister::ListByOwner(const string &owner) const {

    unsigned long index;
    if (binaryByOwner(vOwners, owner, index)) {
        return CIterator(vOwners[index]->v);
    } else {
        vector<Land *> empty;
        return CIterator(empty);
    }
}

#ifndef __PROGTEST__

static void test0(void) {
    CLandRegister x;
    string owner;


    assert(iequals(string(""), string("")) == 0);

    assert(iequals(string(""), string(".")) < 0);
    assert(iequals(string("CVUT"), string("cvut")) == 0);
    assert(iequals(string("cvUt"), string("cvuu")) == -1);

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

void test2() {
    CLandRegister x;
    string owner;

    assert(iequals(string(""), string("=")) < 0);
    assert(iequals(string("="), string("")) > 0);

    assert(x.Add("Prague", "Thakurova", "Dejvice", 12345));
    assert(x.Add("Prague", "Evropska", "Vokovice", 12345));
    assert(x.Add("Prague", "Technicka", "Dejvice", 9873));
    assert(x.Add("Prague", "Technickaa", "Hradcany", 7344));
    assert(x.Add("Brno", "BozetechoVa", "Dejvice", 98735));
    assert(!x.GetOwner("Prague", "THAKUROVA", owner));
    assert(!x.GetOwner("Hradcany", 7343, owner));

    assert(x.NewOwner("Prague", "Thakurova", "="));
    assert(x.NewOwner("Prague", "Technicka", "."));
    assert(x.NewOwner("Brno", "BozetechoVa", "!!"));
    assert(x.NewOwner("Prague", "Thakurova", ""));
}

int main(void) {
    test0();
    test1();
    test2();
    return 0;
}

#endif /* __PROGTEST__ */
