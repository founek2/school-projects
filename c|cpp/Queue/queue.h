//
// Created by Martin Skalický on 08/03/2019.
//
#include <string>

#ifndef QUEUE_QUEUE_H
#define QUEUE_QUEUE_H

struct Node {
    Node(int d = 0) : data(d) {};
    int data;
    Node *left;
    Node *right;
};

class Queue {
public:
    Queue() : root(nullptr) {};

    Queue(const Queue &T);

    ~Queue();

    Queue &operator=(const Queue &T);

    bool isEmpty() const;

    bool insert(int d);

    bool remove(int d);

    bool exists(int d);

    void copy(Node *n);

    Node *root;

private:

    Node *find(int d, Node *&prev) const;

    void print(Node *n);

    void cut(Node *n);
};


#endif //QUEUE_QUEUE_H
