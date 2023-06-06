//
//  main.cpp
//  PA2-03
//
//  Created by Petr Pauš on 3/7/16.
//  Copyright © 2016 Petr Pauš. All rights reserved.
//

#include <iostream>
#include <cassert>
#include "queue.h"
using namespace std;

int main(int argc, const char * argv[]) {

    Queue f;
    int d=0;

    for (int i=0; i<10; i++) f.push(i);
    cout << f.toString() << endl;;
    assert(f.isEmpty()==false);
    assert(f.toString() == "0 1 2 3 4 5 6 7 8 9");

    for (int i=0; i<5; i++) f.pop();
    cout << f.toString() << endl;;
    assert(f.toString() == "5 6 7 8 9");

    for (int i=20; i<25; i++) f.push(i);
    cout << f.toString() << endl;
    assert(f.toString() == "5 6 7 8 9 20 21 22 23 24");

    for (int i=0; i<20; i++) f.pop();
    cout << f.toString() << endl;
    assert(f.isEmpty());
    assert(f.toString() == "");

    assert(f.pop(d)==false);


    for (int i=0; i<10000000; i++) f.push(i);
    assert(f.getLength()==10000000);
   /* assert(f.front()==0);

    assert(f.pop(d));
    assert(d==0);

    assert(Queue::getInst()==1);

    Queue *q = new Queue[30];
    assert(Queue::getInst()==31);

    q[5].push(555);

    delete []q;
    assert(Queue::getInst()==1);*/

    return 0;
}
