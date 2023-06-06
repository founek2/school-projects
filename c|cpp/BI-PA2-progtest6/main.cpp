#ifndef __PROGTEST__

#include <cassert>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <list>
#include <algorithm>
#include <memory>
#include <functional>

using namespace std;
#endif /* __PROGTEST__ */


class CComponent {
public:
    virtual void Print(const string &prefix, std::ostream &os) const = 0;

    virtual unique_ptr<CComponent> Clone() const = 0;

    virtual ~CComponent() {};
};

class CCPU : public CComponent {
public:
    CCPU(unsigned cores, unsigned freq);

    virtual void Print(const string &prefix, std::ostream &os) const;

    virtual unique_ptr<CComponent> Clone() const;

private:
    unsigned m_cores;
    unsigned m_freq;
};

class CMemory : public CComponent {
public:
    CMemory(unsigned size);

    virtual void Print(const string &prefix, std::ostream &os) const;

    virtual unique_ptr<CComponent> Clone() const;

private:
    unsigned m_size;
};

class CDisk : public CComponent {
public:
    CDisk(const string &type, unsigned size);

    CDisk &AddPartition(unsigned size, const string &id);

    virtual void Print(const string &prefix, std::ostream &os) const;

    virtual unique_ptr<CComponent> Clone() const;

    static const string MAGNETIC;
    static const string SSD;

private:
    struct CPartition {
        CPartition(unsigned size, string id) : m_size(size), m_id(id) {};
        unsigned m_size;
        string m_id;
    };

    unsigned m_size;
    string m_type;
    vector<CPartition> m_partitions;
};

class CComputer {
public:
    CComputer(const string &name);

    CComputer(const CComputer &pc);

    template<typename _T>
    CComputer &AddComponent(const _T &);

    CComputer &AddAddress(const string &address);

    bool HasName(const string name);

    friend ostream &operator<<(std::ostream &os, const CComputer &pc);

    void Print(const string &prefix, std::ostream &os) const;

    CComputer &operator=(const CComputer &pc);

private:
    vector<unique_ptr<CComponent>> m_components;
    vector<string> m_address;
    string m_name;
};

class CNetwork {
public:
    CNetwork(const string &name) : m_name(name) {}

    CNetwork &AddComputer(const CComputer &pc);

    CComputer *FindComputer(const string &);

    friend ostream &operator<<(std::ostream &os, const CNetwork &network);

private:
    string m_name;
    vector<CComputer> m_computers;
};

CCPU::CCPU(unsigned cores, unsigned freq) : m_cores(cores), m_freq(freq) {}

void CCPU::Print(const string &prefix, std::ostream &os) const {
    os << "CPU, " << m_cores << " cores @ " << m_freq << "MHz";
}

unique_ptr<CComponent> CCPU::Clone() const {
    return make_unique<CCPU>(*this);
}

CMemory::CMemory(unsigned size) : m_size(size) {}

void CMemory::Print(const string &prefix, std::ostream &os) const {
    os << "Memory, " << m_size << " MiB";
}

unique_ptr<CComponent> CMemory::Clone() const {
    return make_unique<CMemory>(*this);
}

CDisk::CDisk(const string &type, unsigned size) : m_size(size), m_type(type) {}

CDisk &CDisk::AddPartition(unsigned size, const string &id) {
    m_partitions.emplace_back(CPartition(size, id));
    return *this;
}

void CDisk::Print(const string &prefix, std::ostream &os) const {
    os << m_type << ", " << m_size << " GiB" << endl;
    unsigned counter = 0;
    for (const auto &pt: m_partitions) {
        os << prefix;
        if (&pt != &m_partitions.back()) os << "+-";
        else os << "\\-";
        os << "[" << counter++ << "]: " << pt.m_size << " GiB, " << pt.m_id;

        if (&pt != &m_partitions.back()) os << endl;
    }
}

unique_ptr<CComponent> CDisk::Clone() const {
    return make_unique<CDisk>(*this);
}

const string CDisk::SSD = "SSD";
const string CDisk::MAGNETIC = "HDD";

std::ostream &operator<<(std::ostream &os, const CNetwork &network) {
    os << "Network: " << network.m_name << endl;
    for (const auto &pc: network.m_computers) {
        if (&pc != &network.m_computers.back()) {
            os << "+-";
            pc.Print("| ", os);
        } else {
            os << "\\-";
            pc.Print("  ", os);
        }
    }
    return os;
}

CComputer::CComputer(const string &name) : m_name(name) {}

CComputer::CComputer(const CComputer &pc) {
    *this = pc;
}

CComputer &CComputer::AddAddress(const string &address) {
    m_address.emplace_back(address);
    return *this;
}

template<typename _T>
CComputer &CComputer::AddComponent(const _T &comp) {
    m_components.push_back(make_unique<_T>(comp));
    return *this;
}

bool CComputer::HasName(const string name) {
    return m_name == name;
}

void CComputer::Print(const string &prefix, std::ostream &os) const {
    os << "Host: " << m_name << endl;

    for (const auto &addr: m_address) {
        if (m_components.empty() && &addr == &m_address.back()) os << prefix << "\\-";
        else os << prefix << "+-";
        os << addr << endl;
    }

    for (const auto &component: m_components) {
        os << prefix;
        if (&component != &m_components.back()) {
            os << "+-";
            component->Print(prefix + "| ", os);
        } else {
            os << "\\-";
            component->Print(prefix + "  ", os);
        }
        os << endl;
    }
}

std::ostream &operator<<(std::ostream &os, const CComputer &pc) {
    pc.Print("", os);
    return os;
}

CComputer &CComputer::operator=(const CComputer &pc) {
    if (this != &pc) {
        m_address = pc.m_address;
        m_name = pc.m_name;

        m_components.clear();
        for (const auto &component: pc.m_components) {
            m_components.push_back(component->Clone());
        }
    }
    return *this;
}

CNetwork &CNetwork::AddComputer(const CComputer &pc) {
    m_computers.emplace_back(pc);
    return *this;
}

CComputer *CNetwork::FindComputer(const string &name) {
    for (CComputer &pc: m_computers) {
        if (pc.HasName(name)) return &pc;
    }
    return NULL;
}

#ifndef __PROGTEST__

template<typename _T>
string toString(const _T &x) {
    ostringstream oss;
    oss << x;
    return oss.str();
}

int main(void) {
    return 0;
}

#endif /* __PROGTEST__ */
