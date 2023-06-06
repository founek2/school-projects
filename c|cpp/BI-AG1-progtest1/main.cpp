#include <iostream>
#include <list>
#include <queue>

using namespace std;

struct Point {
    char val = 0;
    int number = -1;
    int pos = -1;
};

void loadQue(Point *q, int qLen) {
    for (int i = 0; i < qLen - 1; ++i) {
        cin >> q[i].val;
        q[i].pos = i;
    }
    q[qLen - 1].val = '.';
    q[qLen - 1].pos = qLen - 1;
}

Point *getSecQ(Point *p, Point q1[], Point q2[], int len) {
    return p >= q2 && p < q2 + len ? q1 : q2;
}

void addToQueue(Point *p, int num, queue<Point *> &q) {
    if (p->number == -1 && p->val == '.') {
        p->number = num + 1;
        q.push(p);
    }
}

int findPosition(Point q1[], Point q2[], int len, int jumpSize) {
    q1[0].number = 0;
    queue<Point *> q({&q1[0]});

    while (!q.empty()) {
        Point *item = q.front();
        q.pop();

        if (item->pos == len - 1) return item->number;


        // front
        Point *nextPoint = item + 1;
        addToQueue(nextPoint, item->number, q);


        // back
        if (item->pos > item->number + 1) {
            Point *itemBefore = item - 1;
            addToQueue(itemBefore, item->number, q);
        }

        // jump
        Point *qqq = getSecQ(item, q1, q2, len);
        Point *itemJump = &qqq[item->pos + jumpSize < len ? item->pos + jumpSize : len - 1];
        addToQueue(itemJump, item->number, q);
    }

    return -1;
}

int main() {
    int queuLen, jumpSize = 0;

    cin >> queuLen;
    cin >> jumpSize;
    ++queuLen;  // make space at end

    auto q1 = new Point[queuLen];
    auto q2 = new Point[queuLen];

    loadQue(q1, queuLen);
    loadQue(q2, queuLen);

    int res = findPosition(q1, q2, queuLen, jumpSize);
    cout << res << endl;

    delete[] q1;
    delete[] q2;
    if (res > -1) return 0;

    return 1;
}