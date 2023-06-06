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
    virtual CComponent *Clone() const = 0;
};

class CCPU : public CComponent {
public:
    CCPU(unsigned cores, unsigned freq);

    virtual void Print(const string &prefix, std::ostream &os) const;

    virtual CComponent *Clone() const;

private:
    unsigned m_cores;
    unsigned m_freq;
};

class CMemory : public CComponent {
public:
    CMemory(unsigned size);

    virtual void Print(const string &prefix, std::ostream &os) const;

    virtual CComponent *Clone() const;

private:
    unsigned m_size;
};

class CDisk : public CComponent {
public:
    CDisk(const string &type, unsigned size);

    CDisk &AddPartition(unsigned size, const string &id);

    virtual void Print(const string &prefix, std::ostream &os) const;

    virtual CComponent *Clone() const;

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

    ~CComputer();

    CComputer &AddComponent(const CCPU &);

    CComputer &AddComponent(const CMemory &);

    CComputer &AddComponent(const CDisk &);

    CComputer &AddAddress(const string &address);

    bool HasName(const string name);

    friend ostream &operator<<(std::ostream &os, const CComputer &pc);

    void Print(const string &prefix, std::ostream &os) const;


    CComputer &operator=(const CComputer &pc);

private:
    vector<CComponent *> m_components;
    vector<string> m_address;
    string m_name;

    void DeleteComponents();
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
};

CComponent *CCPU::Clone() const {
    return new CCPU(*this);
}

CMemory::CMemory(unsigned size) : m_size(size) {}

void CMemory::Print(const string &prefix, std::ostream &os) const {
    os << "Memory, " << m_size << " MiB";
};

CComponent *CMemory::Clone() const {
    return new CMemory(*this);
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

CComponent *CDisk::Clone() const {
    return new CDisk(*this);
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

std::ostream &operator<<(std::ostream &os, const CComputer &pc) {
    pc.Print("", os);
    return os;
}

CComputer::CComputer(const string &name) : m_name(name) {}

CComputer::CComputer(const CComputer &pc) {
    /*m_address = pc.m_address;
    m_name = pc.m_name;

    for (const auto &component: pc.m_components) {
        m_components.push_back(component->Clone());
    }*/
    *this = pc;
}

CComputer::~CComputer() {
    DeleteComponents();
}

CComputer &CComputer::AddAddress(const string &address) {
    m_address.emplace_back(address);
    return *this;
}

CComputer &CComputer::AddComponent(const CDisk &comp) {
    m_components.push_back(new CDisk(comp));
    return *this;
}

CComputer &CComputer::AddComponent(const CMemory &comp) {
    m_components.push_back(new CMemory(comp));
    return *this;
}

CComputer &CComputer::AddComponent(const CCPU &comp) {
    m_components.push_back(new CCPU(comp));
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

CComputer &CComputer::operator=(const CComputer &pc) {
    if (this != &pc) {
        m_address = pc.m_address;
        m_name = pc.m_name;

        DeleteComponents();
        for (const auto &component: pc.m_components) {
            m_components.push_back(component->Clone());
        }
    }
    return *this;
}

void CComputer::DeleteComponents() {
    for (auto &component: m_components)
        delete component;
    m_components.clear();
}

#ifndef __PROGTEST__

template<typename _T>
string toString(const _T &x) {
    ostringstream oss;
    oss << x;
    return oss.str();
}

int main(void) {
    CNetwork n("FIT network");
    n.AddComputer(
            CComputer("progtest.fit.cvut.cz").
                    AddAddress("147.32.232.142").
                    AddComponent(CCPU(8, 2400)).
                    AddComponent(CCPU(8, 1200)).
                    AddComponent(CDisk(CDisk::MAGNETIC, 1500).
                    AddPartition(50, "/").
                    AddPartition(5, "/boot").
                    AddPartition(1000, "/var")).
                    AddComponent(CDisk(CDisk::SSD, 60).
                    AddPartition(60, "/data")).
                    AddComponent(CMemory(2000)).
                    AddComponent(CMemory(2000))).
            AddComputer(
            CComputer("edux.fit.cvut.cz").
                    AddAddress("147.32.232.158").
                    AddComponent(CCPU(4, 1600)).
                    AddComponent(CMemory(4000)).
                    AddComponent(CDisk(CDisk::MAGNETIC, 2000).
                    AddPartition(100, "/").
                    AddPartition(1900, "/data"))).
            AddComputer(
            CComputer("imap.fit.cvut.cz").
                    AddAddress("147.32.232.238").
                    AddComponent(CCPU(4, 2500)).
                    AddAddress("2001:718:2:2901::238").
                    AddComponent(CMemory(8000)));
    cout << toString(n);
    assert (toString(n) ==
            "Network: FIT network\n"
            "+-Host: progtest.fit.cvut.cz\n"
            "| +-147.32.232.142\n"
            "| +-CPU, 8 cores @ 2400MHz\n"
            "| +-CPU, 8 cores @ 1200MHz\n"
            "| +-HDD, 1500 GiB\n"
            "| | +-[0]: 50 GiB, /\n"
            "| | +-[1]: 5 GiB, /boot\n"
            "| | \\-[2]: 1000 GiB, /var\n"
            "| +-SSD, 60 GiB\n"
            "| | \\-[0]: 60 GiB, /data\n"
            "| +-Memory, 2000 MiB\n"
            "| \\-Memory, 2000 MiB\n"
            "+-Host: edux.fit.cvut.cz\n"
            "| +-147.32.232.158\n"
            "| +-CPU, 4 cores @ 1600MHz\n"
            "| +-Memory, 4000 MiB\n"
            "| \\-HDD, 2000 GiB\n"
            "|   +-[0]: 100 GiB, /\n"
            "|   \\-[1]: 1900 GiB, /data\n"
            "\\-Host: imap.fit.cvut.cz\n"
            "  +-147.32.232.238\n"
            "  +-2001:718:2:2901::238\n"
            "  +-CPU, 4 cores @ 2500MHz\n"
            "  \\-Memory, 8000 MiB\n");
    CNetwork x = n;
    auto c = x.FindComputer("imap.fit.cvut.cz");
    assert (toString(*c) ==
            "Host: imap.fit.cvut.cz\n"
            "+-147.32.232.238\n"
            "+-2001:718:2:2901::238\n"
            "+-CPU, 4 cores @ 2500MHz\n"
            "\\-Memory, 8000 MiB\n");
    c->AddComponent(CDisk(CDisk::MAGNETIC, 1000).
            AddPartition(100, "system").
            AddPartition(200, "WWW").
            AddPartition(700, "mail"));
    assert (toString(x) ==
            "Network: FIT network\n"
            "+-Host: progtest.fit.cvut.cz\n"
            "| +-147.32.232.142\n"
            "| +-CPU, 8 cores @ 2400MHz\n"
            "| +-CPU, 8 cores @ 1200MHz\n"
            "| +-HDD, 1500 GiB\n"
            "| | +-[0]: 50 GiB, /\n"
            "| | +-[1]: 5 GiB, /boot\n"
            "| | \\-[2]: 1000 GiB, /var\n"
            "| +-SSD, 60 GiB\n"
            "| | \\-[0]: 60 GiB, /data\n"
            "| +-Memory, 2000 MiB\n"
            "| \\-Memory, 2000 MiB\n"
            "+-Host: edux.fit.cvut.cz\n"
            "| +-147.32.232.158\n"
            "| +-CPU, 4 cores @ 1600MHz\n"
            "| +-Memory, 4000 MiB\n"
            "| \\-HDD, 2000 GiB\n"
            "|   +-[0]: 100 GiB, /\n"
            "|   \\-[1]: 1900 GiB, /data\n"
            "\\-Host: imap.fit.cvut.cz\n"
            "  +-147.32.232.238\n"
            "  +-2001:718:2:2901::238\n"
            "  +-CPU, 4 cores @ 2500MHz\n"
            "  +-Memory, 8000 MiB\n"
            "  \\-HDD, 1000 GiB\n"
            "    +-[0]: 100 GiB, system\n"
            "    +-[1]: 200 GiB, WWW\n"
            "    \\-[2]: 700 GiB, mail\n");
    assert (toString(n) ==
            "Network: FIT network\n"
            "+-Host: progtest.fit.cvut.cz\n"
            "| +-147.32.232.142\n"
            "| +-CPU, 8 cores @ 2400MHz\n"
            "| +-CPU, 8 cores @ 1200MHz\n"
            "| +-HDD, 1500 GiB\n"
            "| | +-[0]: 50 GiB, /\n"
            "| | +-[1]: 5 GiB, /boot\n"
            "| | \\-[2]: 1000 GiB, /var\n"
            "| +-SSD, 60 GiB\n"
            "| | \\-[0]: 60 GiB, /data\n"
            "| +-Memory, 2000 MiB\n"
            "| \\-Memory, 2000 MiB\n"
            "+-Host: edux.fit.cvut.cz\n"
            "| +-147.32.232.158\n"
            "| +-CPU, 4 cores @ 1600MHz\n"
            "| +-Memory, 4000 MiB\n"
            "| \\-HDD, 2000 GiB\n"
            "|   +-[0]: 100 GiB, /\n"
            "|   \\-[1]: 1900 GiB, /data\n"
            "\\-Host: imap.fit.cvut.cz\n"
            "  +-147.32.232.238\n"
            "  +-2001:718:2:2901::238\n"
            "  +-CPU, 4 cores @ 2500MHz\n"
            "  \\-Memory, 8000 MiB\n");
    return 0;
}

#endif /* __PROGTEST__ */
