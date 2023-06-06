#ifndef __PROGTEST__
#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <climits>
#include <cfloat>
#include <cassert>
#include <cmath>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <numeric>
#include <vector>
#include <set>
#include <list>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <queue>
#include <stack>
#include <deque>
#include <memory>
#include <functional>
#include <thread>
#include <mutex>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include "progtest_solver.h"
#include "sample_tester.h"
using namespace std;
#endif /* __PROGTEST__ */

unsigned getId(uint64_t & data){
    return data >> 37;
}

class CSentinelHacker
{
  public:
    static bool              SeqSolve                      ( const vector<uint64_t> & fragments,
                                                             CBigInt         & res );

    static bool              SeqSolve2                      ( const vector<uint64_t> & fragments,
                                                             size_t len,
                                                             CBigInt         & res );

    void                     AddTransmitter                ( ATransmitter      x );
    void                     AddReceiver                   ( AReceiver         x );
    void                     AddFragment                   ( uint64_t          x );
    void                     Start                         ( unsigned          thrCount );
    void                     Stop                          ( void );

private:
    std::queue<uint64_t> qFragments;
    std::map<uint32_t, vector<uint64_t>> problems;
    std::queue<std::pair<uint32_t, CBigInt>> qResults;
    std::vector<AReceiver> receivers;
    std::vector<ATransmitter> transmitters;
    std::vector<std::thread> th_workers;
    std::vector<std::thread> th_receivers;
    std::vector<std::thread> th_transmitters;

    std::mutex mut_fragments;
    std::mutex mut_problems;
    std::mutex mut_results;

    bool shouldStop = false;
    bool workersDeath = false;

    std::condition_variable cv_fragment;
    std::condition_variable cv_result;

    void handleReceiver(AReceiver&);

    void handleTransmitter(ATransmitter&);

    void workerMain();

    bool popFragment(uint64_t&);

    bool popResult(std::pair<uint32_t, CBigInt> &);

    bool popIncomplete(uint32_t &);
};
// TODO: CSentinelHacker implementation goes here
//-------------------------------------------------------------------------------------------------
bool CSentinelHacker::SeqSolve(const vector<uint64_t> & fragments,
                          CBigInt         & res ) {
    res = 0;
    const uint32_t count = FindPermutations(fragments.data(), fragments.size(), [&res](const uint8_t * payload, size_t len){
        const uint8_t * bitfield = payload + 32 / 8;
        CBigInt out = CountExpressions(bitfield, len - 32);
        if (out.CompareTo(res))    // out > result -> 1
            res = out;
    });

    return count > 0;
}

bool CSentinelHacker::SeqSolve2(const vector<uint64_t> & fragments, size_t len,
                               CBigInt         & res ) {
    res = 0;
    const uint32_t count = FindPermutations(fragments.data(), len, [&res](const uint8_t * payload, size_t len){
        const uint8_t * bitfield = payload + 32 / 8;
        CBigInt out = CountExpressions(bitfield, len - 32);
        if (out.CompareTo(res))    // out > result -> 1
            res = out;
    });

    return count > 0;
}

void CSentinelHacker::AddTransmitter(ATransmitter x) {
    this->transmitters.emplace_back(x);
}

void CSentinelHacker::AddReceiver(AReceiver x) {
    this->receivers.emplace_back(x);
}

void CSentinelHacker::AddFragment(uint64_t x) {
    std::lock_guard<std::mutex> lockGuard(mut_fragments);
    this->qFragments.push(x);
    cv_fragment.notify_one();
}

void CSentinelHacker::Start(unsigned thrCount) {
    for (auto &it : this->receivers)
        this->th_receivers.emplace_back(&CSentinelHacker::handleReceiver, this, std::ref(it));

    for (auto &it : this->transmitters)
        this->th_transmitters.emplace_back(&CSentinelHacker::handleTransmitter, this, std::ref(it));

    for (unsigned i = 0; i < thrCount; ++i) {
        this->th_workers.emplace_back(&CSentinelHacker::workerMain, this);
    }
}

void CSentinelHacker::Stop() {
    // signalizace konce
    this->shouldStop = true;
    // počká na ukončení všech vláken
    for (auto & th: this->th_receivers){
        th.join();
    }
    for (auto & th: this->th_workers){
        th.join();
    }

    this->workersDeath = true;

    for (auto & th: this->th_transmitters){
        th.join();
    }
}

void CSentinelHacker::handleReceiver(AReceiver & rec) {
    bool state = true;
    uint64_t data;

    while (state) {
        state = rec->Recv(data);
        if (state) this->AddFragment(data);
    }
}

void CSentinelHacker::handleTransmitter(ATransmitter & trans) {
    std::pair<uint32_t, CBigInt> element;

    bool state = true;
    while(state){
        state = this->popResult(element);

        if (state) {
            trans->Send(element.first, element.second);
        }
    }

    uint32_t id;
    while(this->popIncomplete(id)){
        trans->Incomplete(id);
    }
}


void CSentinelHacker::workerMain() {
    uint64_t data;
    while(this->popFragment(data)){
        unsigned id = getId(data);

        std::map<unsigned, vector<uint64_t>>::iterator it;
        size_t len;

        {
            std::lock_guard<std::mutex> lockGuard(mut_problems);
            it = this->problems.find(id);

            if (it == this->problems.end())
                it = this->problems.insert(std::pair<unsigned, vector<uint64_t>>(id, {data})).first;
            else
                it->second.push_back(data);
            len = it->second.size();
        }
        CBigInt out;
        //cout << "SeqSolve2" << std::endl;
        if (this->SeqSolve2(it->second, len, out)){
            {
                // cout << "found solution" << std::endl;
                std::lock_guard<std::mutex> lockGuard2(mut_results);
                this->qResults.push(std::make_pair(it->first, out));
            }
            cv_result.notify_one();
            {
                std::lock_guard<std::mutex> lockGuard(mut_problems);
                this->problems.erase(it);
            }
        }
    }
}

bool CSentinelHacker::popFragment(uint64_t & data) {
    std::unique_lock<std::mutex> lk(this->mut_fragments);

    while  (true) {
        auto now = std::chrono::system_clock::now();
        bool timeOut = !cv_fragment.wait_until(lk, now + std::chrono::milliseconds(100), [this]{return !this->qFragments.empty();});
        if (timeOut && shouldStop) return false;
        else if (timeOut && !shouldStop) continue;
        else break;
    }
    data = this->qFragments.front();
    this->qFragments.pop();

    return true;
}

bool CSentinelHacker::popResult(std::pair<uint32_t, CBigInt> & elem) {
    std::unique_lock<std::mutex> lk(this->mut_results);

    while  (true) {
        auto now = std::chrono::system_clock::now();
        bool finished = cv_result.wait_until(lk, now + std::chrono::milliseconds(100), [this]{return !this->qResults.empty();});
        if (!finished && workersDeath) return false;
        else if (!finished && !workersDeath) continue;
        else break;
    }
    elem = this->qResults.front();
    this->qResults.pop();

    return true;
}

bool CSentinelHacker::popIncomplete(uint32_t & id) {
    std::lock_guard lock(mut_problems);

    if (this->problems.size() == 0) return false;

    auto it = problems.extract(problems.begin());
    id = it.key();
    return true;
}

#ifndef __PROGTEST__
int                main                                    ( void )
{
  using namespace std::placeholders;
  for ( const auto & x : g_TestSets )
  { 
    CBigInt res;
    assert ( CSentinelHacker::SeqSolve ( x . m_Fragments, res ) );
    assert ( CBigInt ( x . m_Result ) . CompareTo ( res ) == 0 );
  }
  
  CSentinelHacker test;
  auto            trans = make_shared<CExampleTransmitter> ();
  AReceiver       recv  = make_shared<CExampleReceiver> ( initializer_list<uint64_t> { 0x02230000000c, 0x071e124dabef, 0x02360037680e, 0x071d2f8fe0a1, 0x055500150755 } );
  
  test . AddTransmitter ( trans ); 
  test . AddReceiver ( recv ); 
  test . Start ( 3 );
  
  static initializer_list<uint64_t> t1Data = { 0x071f6b8342ab, 0x0738011f538d, 0x0732000129c3, 0x055e6ecfa0f9, 0x02ffaa027451, 0x02280000010b, 0x02fb0b88bc3e };
  thread t1 ( FragmentSender, bind ( &CSentinelHacker::AddFragment, &test, _1 ), t1Data );
  
  static initializer_list<uint64_t> t2Data = { 0x073700609bbd, 0x055901d61e7b, 0x022a0000032b, 0x016f0000edfb };
  thread t2 ( FragmentSender, bind ( &CSentinelHacker::AddFragment, &test, _1 ), t2Data );
  FragmentSender ( bind ( &CSentinelHacker::AddFragment, &test, _1 ), initializer_list<uint64_t> { 0x017f4cb42a68, 0x02260000000d, 0x072500000025 } );
  t1 . join ();
  t2 . join ();
  std::this_thread::sleep_for (std::chrono::seconds(10));
  test . Stop ();
  assert ( trans -> TotalSent () == 4 );
  assert ( trans -> TotalIncomplete () == 2 );
  return 0;  
}
#endif /* __PROGTEST__ */ 
