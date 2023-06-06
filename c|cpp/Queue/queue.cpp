//
// Created by Martin Skalický on 08/03/2019.
//

#include "queue.h"
#include <stddef.h>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;

Queue::~Queue() {
    // ble
}

Queue::Queue(const Queue &T) {
    root = nullptr;
    copy(T.root);
}

void Queue::copy(Node *n) {
    if (!n) return;
    insert(n->data);
    copy(n->left);
    copy(n->right);
}

bool Queue::isEmpty() const {
    return !root;
}

Queue &Queue::operator=(const Queue &T) {
    if (this != &T) {
        cut(root);
        root = nullptr;
        copy(T.root);
    }
}

bool Queue::insert(int d) {
    Node *it, *prev = nullptr;
    it = find(d, prev);
    if (!it) {
        it = new Node(d);
        if (!prev) root = it;
        else if (d < prev->data) prev > left = it;
        else prev->right = it;
        return true;
    }
    return false;
}

bool Queue::remove(int d) {
    Node *prev = nullptr;
    Node *it = find(d, prev);
    Node *tmp = nullptr;
    if (it) {
        if (it->left && it->right) {
            prev = it;
            *tmp = it->left;
            while (tmp->right) {
                prev = tmp;
                tmp = tmp->right;
            }
            it->data = tmp->data;
        }
        tmp = tmp ? tmp : it;
        if (prev) {  // nemažeme root
            if (prev->data < tmp->data)
                prev->right = tmp->left;
            else
                prev->left = tmp->left;
        } else root = root->left ? root->left : root->right;//root node mažeme
    }

    delete tmp;
}

bool Queue::exists(int d) {
    Node *prev = nullptr;
    Node *it = find(d, prev);

    return it;
}

void Queue::print(Node *n) {
    if (!n) return;
    print(n->left);
    std::cout << n->data << " ";
    print(n->right);
}

void Queue::cut(Node *n) {
    if (!n)return;
    cut(n->left);
    cut(n->right);
    delete n;
}