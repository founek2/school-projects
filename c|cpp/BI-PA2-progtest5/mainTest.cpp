#include <cassert>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <list>
#include <algorithm>
#include <memory>
#include <functional>
#include <cmath>

using namespace std;

size_t split(const string &txt, char ch, vector<string> &strs, unsigned maxSplit);

class CTimeStamp {
public:
    CTimeStamp(int year,
               int month,
               int day,
               int hour,
               int minute,
               double sec) : m_year(year),
                             m_month(month),
                             m_day(day),
                             m_hour(hour),
                             m_minute(minute),
                             m_sec(sec) {}

    int Compare(const CTimeStamp &x) const {
        ostringstream s1;
        s1 << *this;
        ostringstream s2;
        s2 << x;
        return s1.str() < s2.str();
    }


    friend ostream &operator<<(ostream &os,
                               const CTimeStamp &x) {
        //YYYY-MM-DD HH24:MI:SS.UUU

        os << std::setw(4) << std::setfill('0')
           << x.m_year << "-"
           << std::setw(2) << std::setfill('0')
           << x.m_month << "-" << std::setw(2) << std::setfill('0')
           << x.m_day << " " << std::setw(2) << std::setfill('0')
           << x.m_hour << ":" << std::setw(2) << std::setfill('0')
           << x.m_minute << ":";
        // << x.m_sec;              // na dvě části a doplnit nuly

        //if (x.m_sec < 10) {
        //os << std::setw(5) << std::setfill('0') << setprecision(5) <<x.m_sec;
        int whole = floor(x.m_sec);
        os << std::setw(2) << std::setfill('0') << whole;
        os << "." << right << std::setw(3) << std::setfill('0') << (x.m_sec - whole) * 1000;
        //}
        return os;
    }

private:
    int m_year;
    int m_month;
    int m_day;
    int m_hour;
    int m_minute;
    double m_sec;
};

class CMail {
public:
    CMail(const CTimeStamp &timeStamp,
          const string &from,
          const string &to,
          const string &subject) :
            m_timeStamp(timeStamp),
            m_from(from),
            m_to(to),
            m_subject(subject) {}

    int CompareByTime(const CTimeStamp &x) const {
        return m_timeStamp.Compare(x);
    }

    int CompareByTime(const CMail &x) const {
        return m_timeStamp.Compare(x.getTime());
    }

    const string &From(void) const {
        return m_from;
    }

    const string &To(void) const {
        return m_to;
    }

    const string &Subject(void) const {
        return m_subject;
    }

    const CTimeStamp &TimeStamp(void) const {
        return m_timeStamp;
    }

    friend ostream &operator<<(ostream &os,
                               const CMail &x) {
        //2019-03-29 13:36:13.023 person3@fit.cvut.cz -> user76@fit.cvut.cz, subject: New progtest homework!

        os << x.m_timeStamp << " " << x.m_from << " -> " << x.m_to << ", subject: " << x.m_subject;
        return os;
    }

    CTimeStamp getTime() const {
        return m_timeStamp;
    }

private:
    CTimeStamp m_timeStamp;
    string m_from;
    string m_to;
    string m_subject;
};
// your code will be compiled in a separate namespace
namespace MysteriousNamespace {
//----------------------------------------------------------------------------------------
    map<string, int> monthsMap
            {
                    {"Jan", 1},
                    {"Feb", 2},
                    {"Mar", 3},
                    {"Apr", 4},
                    {"May", 5},
                    {"Jun", 6},
                    {"Jul", 7},
                    {"Aug", 8},
                    {"Sep", 9},
                    {"Oct", 10},
                    {"Nov", 11},
                    {"Dec", 12}
            };

    unsigned getMonthNum(string month) {
        const auto it = monthsMap.find(month);
        if (it != monthsMap.cend())
            return it->second;
        throw invalid_argument("Invalid syntax.");
    }

    CTimeStamp createCTimeStamp(vector<string> fields, vector<string> time) {
        return CTimeStamp(
                stoi(fields[2]),
                getMonthNum(fields[0]),
                stoi(fields[1]),
                stoi(time[0]),
                stoi(time[1]),
                stod(time[2])
        );
    }

    int cmpF(const CMail &mail, const CTimeStamp &time) {
        return mail.CompareByTime(time);
    }

    struct CMailData {
        CMailData(string &from, string subject = "") : m_from(from), m_subject(subject) {}

        vector<pair<string, CTimeStamp>> m_to;
        string m_from;
        string m_subject;

        friend ostream &operator<<(ostream &os,
                                   const CMailData &x) {
            os << "from=" << x.m_from << ", subject=" << x.m_subject << endl;
            for (auto const &it: x.m_to) {
                os << "to=" << it.first << ", " << it.second << endl;
            }
            return os;
        }
    };

    size_t split(const string &txt, char ch, vector<string> &strs, unsigned maxSplit) {
        size_t pos = txt.find(ch);
        size_t initialPos = 0;
        strs.clear(); // vyčistí vektor

        size_t counter = 0;
        while (pos != string::npos) {
            strs.push_back(txt.substr(initialPos, pos - initialPos));
            initialPos = pos + 1;

            pos = txt.find(ch, initialPos);
            ++counter;
            if (counter == maxSplit) break;
        }

        // Add the last one
        strs.push_back(txt.substr(initialPos, txt.size() - initialPos));

        return strs.size();
    }

    class CMailLog {
    public:
        int ParseLog(istream &in);

        list<CMail> ListMail(const CTimeStamp &from,
                             const CTimeStamp &to) const;

        set<string> ActiveUsers(const CTimeStamp &from,
                                const CTimeStamp &to) const;

    private:
        vector<CMail> emails;
    };

    int CMailLog::ParseLog(istream &in) {
        string line;
        vector<string> fields;
        vector<string> actions;
        vector<string> time;

        map<string, CMailData> mp;
        int mailCounter = 0;

        while (getline(in, line)) {
            // filter rows with columns < 7
            try {
                if (split(line, ' ', fields, 6) == 7) {
                    /* string delim;
                     for (const auto &str : fields) {
                         cout << delim << str;
                         delim = ", ";
                     }
                     cout << endl;*/

                    // filter rows without = in actions column
                    if (split(fields[6], '=', actions, 1) == 2) {
                        string *id = &(fields[5]);
                        // pouze povolené akce
                        if (actions[0] == "from") {
                            mp.insert(make_pair(*id, CMailData(actions[1])));
                        } else if (actions[0] == "to") {
                            if (split(fields[3], ':', time, 2) == 3) {
                                auto it = mp.find(*id);

                                it->second.m_to.push_back(make_pair(actions[1],
                                                                    createCTimeStamp(fields, time)
                                ));
                            }
                            ++mailCounter;
                        } else if (actions[0] == "subject") {
                            auto it = mp.find(*id);
                            it->second.m_subject = actions[1];
                        }
                    }
                }
            } catch (const std::exception &e) {}
        }
/*        for (auto const &x: mp) {
            cout << x.first // string (key)
                 << ' '
                 << x.second // string's value
                 << std::endl;
        }*/

        emails.reserve(emails.size() + mp.size());
        for (const auto &pair: mp) {
            for (const auto &toPair: pair.second.m_to)
                emails.emplace_back(toPair.second, pair.second.m_from, toPair.first, pair.second.m_subject);
        }
        sort(emails.begin(), emails.end(), [](const CMail &mail1, const CMail &mail2) {
            return mail1.CompareByTime(mail2);
        });

        /*for (auto const &x: emails) {
            cout << x << endl;
        }*/
        /*emails.sort( [] (const CMail &mail1, const CMail & mail2) {
            return mail1.CompareByTime(mail2);
        });*/

        return mailCounter;
    }

    list<CMail> CMailLog::ListMail(const CTimeStamp &from, const CTimeStamp &to) const {
        const auto first = lower_bound(emails.begin(), emails.end(), from, cmpF);
        const auto last = lower_bound(emails.begin(), emails.end(), to, cmpF);

        if (last == emails.end()) return list<CMail>(first, last);
        return list<CMail>(first, last + 1);
    }

    set<string> CMailLog::ActiveUsers(const CTimeStamp &from, const CTimeStamp &to) const {
        const auto first = lower_bound(emails.begin(), emails.end(), from, cmpF);
        const auto last = lower_bound(emails.begin(), emails.end(), to, cmpF);

        const auto lastEnd = last != emails.end() ? last : emails.end();
        set<string> users;
        for (auto it = first; it != lastEnd; ++it) {
            users.insert(it->From());
            users.insert(it->To());
        }

        return users;
    }
//----------------------------------------------------------------------------------------
} // namespace
string printMail(const list<CMail> &all) {
    ostringstream oss;
    for (const auto &mail : all)
        oss << mail << endl;
    return oss.str();
}

string printUsers(const set<string> &all) {
    ostringstream oss;
    bool first = true;
    for (const auto &name : all) {
        if (!first)
            oss << ", ";
        else
            first = false;
        oss << name;
    }
    return oss.str();
}

int main(void) {
    MysteriousNamespace::CMailLog m;
    list<CMail> mailList;
    set<string> users;
    istringstream iss;

    iss.clear();
    iss.str(
            "Mar 29 2019 12:35:32.233 relay.fit.cvut.cz ADFger72343D: from=user1@fit.cvut.cz\n"
            "Mar 29 2019 12:37:16.234 relay.fit.cvut.cz JlMSRW4232Df: from=person3@fit.cvut.cz\n"
            "Mar 29 2019 12:55:13.023 relay.fit.cvut.cz JlMSRW4232Df: subject=New progtest homework!\n"
            "Mar 29 2019 13:38:45.043 relay.fit.cvut.cz Kbced342sdgA: from=office13@fit.cvut.cz\n"
            "Mar 29 2019 13:36:13.023 relay.fit.cvut.cz JlMSRW4232Df: to=user76@fit.cvut.cz\n"
            "Mar 29 2019 13:55:31.456 relay.fit.cvut.cz KhdfEjkl247D: from=PR-department@fit.cvut.cz\n"
            "Mar 29 2019 14:18:12.654 relay.fit.cvut.cz Kbced342sdgA: to=boss13@fit.cvut.cz\n"
            "Mar 29 2019 14:48:32.563 relay.fit.cvut.cz KhdfEjkl247D: subject=Business partner\n"
            "Mar 29 2019 14:58:32.000 relay.fit.cvut.cz KhdfEjkl247D: to=HR-department@fit.cvut.cz\n"
            "Mar 29 2019 14:25:23.233 relay.fit.cvut.cz ADFger72343D: mail undeliverable\n"
            "Mar 29 2019 15:02:34.231 relay.fit.cvut.cz KhdfEjkl247D: to=CIO@fit.cvut.cz\n"
            "Mar 29 2019 15:02:34.230 relay.fit.cvut.cz KhdfEjkl247D: to=CEO@fit.cvut.cz\n"
            "Mar 29 2019 15:02:34.230 relay.fit.cvut.cz KhdfEjkl247D: to=dean@fit.cvut.cz\n"
            "Mar 29 2019 15:02:34.230 relay.fit.cvut.cz KhdfEjkl247D: to=vice-dean@fit.cvut.cz\n"
            "Mar 29 2019 15:02:34.230 relay.fit.cvut.cz KhdfEjkl247D: to=archive@fit.cvut.cz\n");
    assert(m.ParseLog(iss) == 8);
    mailList = m.ListMail(CTimeStamp(2019, 3, 28, 0, 0, 0),
                          CTimeStamp(2019, 3, 29, 23, 59, 59));

    assert(printMail(mailList) ==
           "2019-03-29 13:36:13.023 person3@fit.cvut.cz -> user76@fit.cvut.cz, subject: New progtest homework!\n"
           "2019-03-29 14:18:12.654 office13@fit.cvut.cz -> boss13@fit.cvut.cz, subject: \n"
           "2019-03-29 14:58:32.000 PR-department@fit.cvut.cz -> HR-department@fit.cvut.cz, subject: Business partner\n"
           "2019-03-29 15:02:34.230 PR-department@fit.cvut.cz -> CEO@fit.cvut.cz, subject: Business partner\n"
           "2019-03-29 15:02:34.230 PR-department@fit.cvut.cz -> dean@fit.cvut.cz, subject: Business partner\n"
           "2019-03-29 15:02:34.230 PR-department@fit.cvut.cz -> vice-dean@fit.cvut.cz, subject: Business partner\n"
           "2019-03-29 15:02:34.230 PR-department@fit.cvut.cz -> archive@fit.cvut.cz, subject: Business partner\n"
           "2019-03-29 15:02:34.231 PR-department@fit.cvut.cz -> CIO@fit.cvut.cz, subject: Business partner\n");
    mailList = m.ListMail(CTimeStamp(2019, 3, 28, 0, 0, 0),
                          CTimeStamp(2019, 3, 29, 14, 58, 32));
    assert(printMail(mailList) ==
           "2019-03-29 13:36:13.023 person3@fit.cvut.cz -> user76@fit.cvut.cz, subject: New progtest homework!\n"
           "2019-03-29 14:18:12.654 office13@fit.cvut.cz -> boss13@fit.cvut.cz, subject: \n"
           "2019-03-29 14:58:32.000 PR-department@fit.cvut.cz -> HR-department@fit.cvut.cz, subject: Business partner\n");
    mailList = m.ListMail(CTimeStamp(2019, 3, 30, 0, 0, 0),
                          CTimeStamp(2019, 3, 30, 23, 59, 59));
    assert(printMail(mailList) == "");
    users = m.ActiveUsers(CTimeStamp(2019, 3, 28, 0, 0, 0),
                          CTimeStamp(2019, 3, 29, 23, 59, 59));
    assert(printUsers(users) ==
           "CEO@fit.cvut.cz, CIO@fit.cvut.cz, HR-department@fit.cvut.cz, PR-department@fit.cvut.cz, archive@fit.cvut.cz, boss13@fit.cvut.cz, dean@fit.cvut.cz, office13@fit.cvut.cz, person3@fit.cvut.cz, user76@fit.cvut.cz, vice-dean@fit.cvut.cz");
    users = m.ActiveUsers(CTimeStamp(2019, 3, 28, 0, 0, 0),
                          CTimeStamp(2019, 3, 29, 13, 59, 59));
    assert(printUsers(users) == "person3@fit.cvut.cz, user76@fit.cvut.cz");
    return 0;
}

