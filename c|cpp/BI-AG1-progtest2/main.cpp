#include <iostream>
#include <climits>

#ifndef __PROGTEST__

#include <cassert>

#endif /* __PROGTEST__ */

class CChain;

class BinaryHeap;

template<class T>
class Node {
public:
    Node(T val) : data(val) {}

    Node *p = nullptr;
    T data;
    unsigned int degree = 0;
    Node *child = nullptr;
    Node *sibling = nullptr;

    void print(std::ostream &out, bool printSibling = true) const {
        out << data << " ";
        if (printSibling && sibling) sibling->print(out);
        else out << std::endl;

        if (child) child->print(out);
    }
};


template<class T>
class BinomHeap {
    using NodeT = Node<T>;
    NodeT *head = nullptr;
    NodeT * minPtr;
    NodeT * endPtr;

    friend std::ostream &operator<<(std::ostream &out, const BinomHeap<T> &c) {
        Node<T> *current = c.head;
        while (current) {
            current->print(out, false);
            out << "-------------" << std::endl;
            current = current->sibling;
        }
        return out;
    }

    void deleteNode(NodeT *node) {
        if (node == nullptr) return;
        deleteNode(node->sibling);
        deleteNode(node->child);
        delete node;
        //std::cout << "dealoc of node" << std::endl;
    }

    void updateMin(NodeT * node) {
        if (minPtr == nullptr || minPtr->data > node->data)
            minPtr = node;
    }

    void updateMin() {
        NodeT * minimum;
        if (!findMinPtr(&minimum)) minPtr = nullptr;
        else minPtr = minimum;
    }

    void linkToEnd(NodeT * node) {
        if (!head) {
            head = node;
            endPtr = node;
        }else {
            endPtr->sibling = node;
            endPtr = endPtr->sibling;
        }
        endPtr->sibling = nullptr;
    }
public:
    BinomHeap(): minPtr(nullptr), endPtr(nullptr) {};

    BinomHeap(NodeT *node) : head(node), minPtr(node), endPtr(node) {}

    void Destroy() {
        deleteNode(head);
    }

    static void mergeTrees(NodeT *t1, NodeT *t2) {
        t2->p = t1;
        t2->sibling = t1->child;
        t1->child = t2;

        t1->degree += 1;
    }

    static NodeT * mergeTreesRet(NodeT * curr, NodeT * next) {
        if (curr->data <= next->data) {
            mergeTrees(curr, next);
            return curr;
        } else {
            mergeTrees(next, curr);
            return next;
        }
    }

    void insert(NodeT data) {
        NodeT  * node = new NodeT(data);
        BinomHeap<T> heap(node);
        merge(heap);
    }

    void merge2(BinomHeap &heap) {
        if (heap.head == nullptr) return;
        updateMin(heap.minPtr);
        if (head == nullptr) {
            head = heap.head;
            heap.head = nullptr;

            return;
        }

        NodeT *first = head;
        NodeT *second = heap.head;
        NodeT *third = nullptr;
        NodeT *newHead = nullptr;

        if (first->degree <= second->degree) {
            third = first;
            first = first->sibling;
        } else {
            third = second;
            second = second->sibling;
        }
        newHead = third;

        while (first != nullptr && second != nullptr) {
            if (first->degree <= second->degree) {
                third->sibling = first;
                first = first->sibling;
            } else {
                third->sibling = second;
                second = second->sibling;
            }

            third = third->sibling;
        }

        if (first != nullptr) { // "nakopírovat" zbytek první haldy
            while (first != nullptr) {
                third->sibling = first;
                first = first->sibling;
                third = third->sibling;
            }
        }

        if (second != nullptr) { // "nakopírovat" zbytek druhé haldy
            while (second != nullptr) {
                third->sibling = second;
                second = second->sibling;
                third = third->sibling;
            }
        }

        NodeT *curr = newHead;
        NodeT *prev = nullptr;
        NodeT *next = curr->sibling;

        while (next != nullptr) {
            if (curr->degree != next->degree || (next->sibling != nullptr && next->sibling->degree == curr->degree)) {
                prev = curr;
                curr = next;
            } else {
                if (curr->data <= next->data) {
                    curr->sibling = next->sibling;
                    mergeTrees(curr, next);
                } else {
                    if (prev == nullptr)
                        newHead = next;
                    else
                        prev->sibling = next;

                    mergeTrees(next, curr);
                    curr = next;
                }
            }
            next = curr->sibling;
        }
        heap.head = nullptr;
        heap.minPtr = nullptr;
        head = newHead;
    }

    void merge(BinomHeap &heap) {
        if (heap.head == nullptr) return;
        updateMin(heap.minPtr);
        if (head == nullptr) {
            head = heap.head;
            heap.head = nullptr;
            heap.minPtr = nullptr;

            return;
        }

        NodeT * summands[3];
        NodeT * carry = nullptr;
        int nonVoid = 2;
        unsigned int currDegree = 0;

        NodeT * first = head;
        NodeT * second = heap.head;

        BinomHeap newHeap;

        while(nonVoid >= 2) {
            nonVoid = 0;
            int count = 0;

            if (first != nullptr) {
                nonVoid++;
                if (first->degree == currDegree) {
                    summands[count++] = first;
                    first = first->sibling;
                }
            }

            if (second != nullptr) {
                nonVoid++;
                if (second->degree == currDegree) {
                    summands[count++] = second;
                    second = second->sibling;
                }
            }
            if (carry != nullptr) {
                nonVoid++;
                summands[count++] = carry;
                carry = nullptr;
            }

            if (count == 3) {
                newHeap.linkToEnd(summands[2]);
                carry = mergeTreesRet(summands[0], summands[1]);
            } else if(count == 2) {
                carry = mergeTreesRet(summands[0], summands[1]);
            }else if(count == 1) {
                newHeap.linkToEnd(summands[0]);
            }

            currDegree += 1;
        }

        while(first != nullptr) {
            NodeT * tmp = first->sibling;
            newHeap.linkToEnd(first);
            first = tmp;
        }

        while(second != nullptr) {
            NodeT * tmp = second->sibling;
            newHeap.linkToEnd(second);
            second = tmp;
        }

        head = newHeap.head;
        heap.head = nullptr;
        heap.minPtr = nullptr;
    }

    bool findMinPtr(NodeT **out) {
        NodeT *ancestor;
        if (!findMinAncestor(&ancestor)) return false;
        if (ancestor == nullptr)
            *out = head;
        else
            *out = ancestor->sibling;
        return true;
    }

    bool findMin(T &out) {
        if (minPtr == nullptr) return false;
        out = T(minPtr->data);
        return true;
    }

    /*
     * false - empty heap
     * ancestor == null -> min is head
     */
    bool findMinAncestor(NodeT **ancestor) {
        if (!head) return false;

        NodeT *node = head;
        T min = node->data;
        NodeT *prev = nullptr;
        *ancestor = nullptr;
        while (node != nullptr) {
            if (node->data < min) {
                *ancestor = prev;
                min = node->data;
            }
            prev = node;
            node = node->sibling;
        }
        return true;
    }

    bool extractMin(T &output) {

        NodeT *ancestor = nullptr;
        if (!findMinAncestor(&ancestor)) return false;

        NodeT *min;
        if (ancestor == nullptr) {
            min = head;
            head = head->sibling;
        } else {
            min = ancestor->sibling;
            ancestor->sibling = min->sibling;
        }


        output = T(min->data);
        //std::cout << min << std::endl;

        NodeT *child = min->child;
        delete min;
        if (child != nullptr) {
            BinomHeap heap = reverseHeapOrder(child);
            merge(heap);
        }
        updateMin();
        return true;
    }

    BinomHeap reverseHeapOrder(BinomHeap heap) {

        NodeT *first = heap.head;
        first->p = nullptr; // unlink possible parent
        NodeT *second = first->sibling;
        if (second) {
            first->sibling = nullptr; // první bude poslední -> na nikoho už nemá neukazuje
            while (second) {
                second->p = nullptr;    // odeber rodiče
                NodeT *next = second->sibling;  // koukni na třetí (na konci bude null)
                second->sibling = first;    // provaž druhý s prvním
                first = second; // posuň se
                second = next; // posuň se
            }
            return first;
        } else return heap;
    }
};


/*
 *
 */
class CGoods {
public:
    unsigned int id;
    unsigned int revenue;

    CGoods() : id(0), revenue(0) {}

    // {revenue: 10, id: 15}    {revenue: 10, id: 14}
    friend bool operator<(const CGoods &g1, const CGoods &g2) {
        return g1.revenue < g2.revenue
               || (g1.revenue == g2.revenue && g1.id < g2.id);
    }

    friend bool operator>(const CGoods &g1, const CGoods &g2) {
        return g1.revenue > g2.revenue
               || (g1.revenue == g2.revenue && g1.id > g2.id);
    }

    friend bool operator<=(const CGoods &g1, const CGoods &g2) {
        return g1.revenue < g2.revenue
               || (g1.revenue == g2.revenue && g1.id <= g2.id);
    }

    CGoods(unsigned int paramId, unsigned int paramRevenue) : id(paramId), revenue(paramRevenue) {}
};

/*  Řetězec
 *  Info:
 *      - svoje id
 *      - binomiální haldu s produkty
 *      - nejnižší revenue z haldy -> při každé změně haldy je třeba aktualizovat
 */
class CChain {
public:
    int m_id;
    CGoods m_lowestGoods;
    unsigned int m_heapPos;
    BinomHeap<CGoods> m_heap;

    explicit CChain(int id) : m_id(id), m_lowestGoods(1, UINT_MAX), m_heapPos(0) {}

    ~CChain() {
        m_heap.Destroy();
    }

    void Add(unsigned int id, unsigned int revenue) {
        m_heap.insert(CGoods(id, revenue));
        updateMinRev();
    }

    void merge(CChain *srcChain) {
        m_heap.merge(srcChain->m_heap);
        // updateMinRev();
    }

    void updateMinRev() {
        CGoods min;
        if (!m_heap.findMin(min))
            m_lowestGoods.revenue = UINT_MAX;    // TODO každé volání bude trvat počet binom stromů.... chce to lepší řešení ze strany haldy
        else m_lowestGoods = min;
    }

    bool Remove(unsigned int &id) {
        CGoods min;
        if (!m_heap.extractMin(min)) return false;
        id = min.id;

        //std::cout << "removing with id=" << min.id << ", " << "rev=" << min.revenue << std::endl;
        updateMinRev();
        return true;
    }

    bool operator<(const CChain &c) const {
        return m_lowestGoods.revenue < c.m_lowestGoods.revenue
               || (m_lowestGoods.revenue == c.m_lowestGoods.revenue && m_id < c.m_id)
               || (m_lowestGoods.revenue == c.m_lowestGoods.revenue && m_id == c.m_id &&
                   m_lowestGoods.id < c.m_lowestGoods.id);
    }

    bool operator>(const CChain &c) const {
        return m_lowestGoods.revenue > c.m_lowestGoods.revenue
               || (m_lowestGoods.revenue == c.m_lowestGoods.revenue && m_id > c.m_id)
               || (m_lowestGoods.revenue == c.m_lowestGoods.revenue && m_id == c.m_id &&
                   m_lowestGoods.id > c.m_lowestGoods.id);
    }
};

class BinaryHeap {
    CChain **array;
    unsigned int length;
    unsigned int index;

    void bubbleUp(unsigned int i) {
        unsigned int parent = getParent(i);
        while (parent != 0 && *array[parent] > *array[i]) {
            swap(parent, i);

            i = parent;
            parent = getParent(parent);
        }
    }

    void bubbleDown(unsigned int i) {
        while (true) {      // zaseknutí asi nezpůsobuje
            unsigned int lChild = getLChild(i);
            unsigned int rChild = getRChild(i);

            if (lChild <= index && rChild <= index) {    // oba potomci jsou
                unsigned int target = *array[rChild] < *array[lChild] ? rChild : lChild;  // porovnání by mělo být ok
                if (*array[i] > *array[target]) {
                    swap(target, i);
                    i = target;
                } else break;
            } else if (lChild <= index && *array[i] > *array[lChild]) {
                swap(i, lChild);
                i = lChild;
            } else if (rChild <= index && *array[i] > *array[rChild]) {
                swap(i, rChild);
                i = rChild;
            } else break;
        }

    }

    void swap(unsigned int i1, unsigned int i2) {
        CChain *tmp = array[i1];
        array[i1] = array[i2];
        array[i1]->m_heapPos = i1;
        array[i2] = tmp;
        tmp->m_heapPos = i2;
    }

    unsigned int getParent(unsigned int i) {
        return i / 2;
    }

    unsigned int getLChild(unsigned int i) {
        return 2 * i;
    }

    unsigned int getRChild(unsigned int i) {
        return 2 * i + 1;
    }

    friend std::ostream &operator<<(std::ostream &out, const BinaryHeap &c) {
        unsigned int j = 1;
        int exp = 1;
        for (unsigned int i = 1; i < c.index + 20; i += exp) {
            exp *= 2;
            for (; j <= i; ++j) {
                std::cout << c.array[j] << " ";
            }
            std::cout << std::endl;
        }

        for (unsigned int k = 0; k <= c.index; ++k)
            out << c.array[k] << ", ";
        out << std::endl;
        return out;
    }

public:
    BinaryHeap() {
        length = 20;
        index = 0;
        array = (CChain **) malloc(length * sizeof(CChain *));
    }

    ~BinaryHeap() {
        free(array);
    }

    void insert(CChain *val) {
        if (index + 1 == length) {
            length *= 2;
            array = (CChain **) realloc(array, length * sizeof(CChain *));
        }
        array[++index] = val;
        val->m_heapPos = index;
        bubbleUp(index);
    }

    bool extractMin(CChain **min) {
        if (index == 0) return false;
        *min = array[1];
        array[1] = array[index];
        index -= 1;

        (*min)->m_heapPos = 0;
        (*min)->m_lowestGoods.revenue = UINT_MAX;

        if (index > 0) bubbleDown(1);
        return true;
    }

    CChain *findMin() {
        return index == 0 ? nullptr : array[1];
    }

    void bubble(unsigned int i) {
        unsigned int parent = getParent(i);
        if (parent != 0 && *array[i] < *array[parent])
            bubbleUp(i);
        else bubbleDown(i);
    }

    unsigned int getSize() {
        return index;
    }
};

// TODO zaměřit se na bin heap
class CHolding {
    CChain **m_array;
    BinaryHeap m_heap;
    int holdingLen = 10001;

    void removeFromHeap(CChain *chain) {
        if (!isInHeap(chain)) return;  // pokud na haldě není -> neodebírej

        chain->m_lowestGoods.revenue = 0;
        m_heap.bubble(chain->m_heapPos);
        CChain *min;
        m_heap.extractMin(&min);    // TODO mužu zkusit na min to nastavovat....
        chain->m_lowestGoods.revenue = UINT_MAX;
        chain->m_heapPos = 0;
    }

    void bubbleOrRemove(CChain *chain) {
        if (chain->m_lowestGoods.revenue == UINT_MAX) removeFromHeap(chain);
        else m_heap.bubble(chain->m_heapPos);
    }

    bool isInHeap(CChain *chain) {
        return chain->m_heapPos != 0;
    }

public:
    CHolding() {
        m_array = new CChain *[holdingLen];
        for (int i = 0; i < holdingLen; ++i)
            m_array[i] = nullptr;
    }

    ~CHolding() {
        for (int i = 0; i < holdingLen; ++i) {
            if (m_array[i] != nullptr) delete m_array[i];
        }
        delete[] m_array;
    }

    void Add(int chain, unsigned int id, unsigned int revenue) {
        CChain * myChain = m_array[chain];
        if (myChain == nullptr){
            myChain = new CChain(chain);
            m_array[chain] = myChain;
        }

        (myChain)->Add(id, revenue);

        if (!isInHeap(myChain)) {   // Není v haldě -> jen vložit
            m_heap.insert(myChain);
        } else m_heap.bubble(myChain->m_heapPos); // už je v haldě -> vyvážit, protože se mu mohla změnit minRevenue
    }

    bool Remove(int chain, unsigned int &id) {
        if (m_array[chain] == nullptr) return false;
        if (!m_array[chain]->Remove(id)) return false;

        bubbleOrRemove(m_array[chain]);
        return true;
    }

    bool Remove(unsigned int &id) {
        CChain *minChain = m_heap.findMin();    // m_array[1]
        if (!minChain) return false;
        if (!minChain->Remove(id)) return false;

        bubbleOrRemove(minChain);
        return true;
    }

    void Merge(int dstChain, int srcChain) {
        if (dstChain == srcChain) return;

        if (!m_array[dstChain]) m_array[dstChain] = new CChain(dstChain);
        if (!m_array[srcChain]) m_array[srcChain] = new CChain(srcChain);


        m_array[dstChain]->merge(m_array[srcChain]);
        removeFromHeap(m_array[srcChain]);

        if (!isInHeap(m_array[dstChain])) m_heap.insert(m_array[dstChain]);
        else
            bubbleOrRemove(m_array[dstChain]);
    }

    bool Merge(void) {
        if (m_heap.getSize() < 2) return false;
        CChain *first = nullptr;
        m_heap.extractMin(&first);
        CChain *sec = nullptr;
        m_heap.extractMin(&sec);
        if (first->m_id > sec->m_id) {
            CChain * tmp = first;
            first = sec;
            sec = tmp;
        }
        first->merge(sec);
        m_heap.insert(first);

        return true;
    }
};

#ifndef __PROGTEST__

void progTest1();

void progTest2();

void progTest3();

void progTest4();

void smazatTest();

void testSmazat();

void test2();

void test3();

void tests();

// marast 3, 4, 9, 11, 16, 15, 17, 14, 12, 30, 26, 19, 29, 28, 25, 27, 27, 20
int main() {
    CHolding m1;
    m1.Add(1, 1, 1);

    m1.Add(2, 1, 1);
    m1.Add(2, 1, 1);

    m1.Add(3, 1, 1);
    m1.Add(3, 1, 1);
    m1.Add(3, 1, 1);

    m1.Add(4, 1, 1);
    m1.Add(4, 1, 1);
    m1.Add(4, 1, 1);
    m1.Add(4, 1, 1);

    assert(m1.Merge());
    assert(m1.Merge());
    assert(m1.Merge());
    assert(!m1.Merge());

    unsigned int id;
    CHolding fX;
    fX.Add(3, 4, 100);
    fX.Add(3, 5, 101);
    fX.Add(3, 10, 102);
    fX.Merge(1, 3);

    CHolding h;

    CHolding f16;
/* CH ID REV */
    f16 . Add(1, 100, 3);
    f16 . Add(2, 200, 2);
    f16 . Add(3, 300, 5);
    f16 . Merge();
    assert( f16 . Remove(1, id) && id == 200);
    assert( f16 . Remove(id) && id == 100);
    assert( f16 . Remove(id) && id == 300);
/*    h.Add(3, 4, 100);
    h.Add(3, 4, 101);
    h.Add(3, 4, 102);
    h.Add(3, 4, 99);

    assert(h.Remove(3, id) == true);
    assert(id == 4);

    assert(h.Remove(3, id) == true);
    assert(id == 4);

    assert(h.Remove(3, id) == true);
    assert(id == 4);

    assert(h.Remove(3, id) == true);
    assert(id == 4);

    assert(h.Remove(3, id) == false);*/

    h.Add(2, 4, 101);
    h.Add(2, 4, 100);
    h.Add(2, 5, 150);
    h.Add(2, 4, 100);

    h.Add(3, 4, 101);
    h.Add(3, 5, 99);

    assert(h.Remove(3, id) == true);
    assert(id == 5);
    assert(h.Remove(3, id) == true);
    assert(id == 4);
    assert(h.Remove(3, id) == false);

    assert(h.Remove(2, id) == true);
    assert(id == 4);
    assert(h.Remove(2, id) == true);
    assert(id == 4);
    assert(h.Remove(2, id) == true);
    assert(id == 4);
    assert(h.Remove(2, id) == true);
    assert(id == 5);
    assert(h.Remove(2, id) == false);

    h.Add(2, 4, 101);
    h.Add(2, 4, 100);
    h.Add(2, 5, 150);
    h.Add(2, 4, 100);

    h.Add(3, 4, 101);
    h.Add(3, 5, 99);

    h.Merge(2, 3);
    assert(h.Remove(3, id) == false);

    assert(h.Remove(2, id) == true);
    assert(id == 5);
    assert(h.Remove(2, id) == true);
    assert(id == 4);

    assert(h.Remove(2, id) == true);
    assert(id == 4);
    assert(h.Remove(2, id) == true);
    assert(id == 4);
    assert(h.Remove(2, id) == true);
    assert(id == 4);
    assert(h.Remove(2, id) == true);
    assert(id == 5);

    assert(h.Remove(2, id) == false);


    h.Add(2, 4, 101);
    h.Add(2, 4, 100);
    h.Add(2, 9, 98);
    h.Add(2, 4, 100);

    h.Add(3, 4, 101);
    h.Add(3, 5, 99);

    assert(h.Remove(id));
    assert(id == 9);

    assert(h.Remove(id));
    assert(id == 5);

    h.Add(2, 4, 100);
    h.Add(2, 10, 4);

    assert(h.Remove(id));
    assert(id == 10);

    //h.Add(10, 9, 4);


    BinomHeap<int> heap;
    heap.insert(5);
    heap.insert(34);
    heap.insert(12);
    heap.insert(33);

    BinomHeap<int> heap2;
    heap2.insert(18);
    heap2.insert(30);
    heap2.insert(23);
    heap2.insert(25);
    // std::cout << heap2;

    heap.merge(heap2);
    //std::cout << heap;

    int min;
    heap.extractMin(min);
    //std::cout << heap;

/*    BinaryHeap<int> h;
    h.insert(19);
    h.insert(27);
    h.insert(11);
    h.insert(14);
    h.insert(26);
    h.insert(15);
    h.insert(28);
    h.insert(9);
    h.insert(20);
    h.insert(30);
    h.insert(16);
    h.insert(3);
    h.insert(2);
    h.insert(17);
    h.insert(25);
    h.insert(1);
    h.insert(12);
    h.insert(29);
    h.insert(27);

    //std::cout << h << "-----------" << std::endl;
    h.extractMin();
    h.extractMin();

    //std::cout << h << "-----------"<< std::endl;

    h.insert(4);*/
    //std::cout << h << "-----------"<< std::endl;    // 0, 3, 4, 9, 11, 16, 15, 17, 14, 12, 30, 26, 19, 29, 28, 25, 27, 27, 20,
    progTest1();
    progTest2();

    progTest3();

    progTest4();

    //tests();
    //test2();
    //test3();
    //smazatTest();
    return 0;
}

bool res;
unsigned int id;

void progTest1() {
//Ukazkovy vstup #1
//-----------------
    CHolding f1;
    f1.Add(7, 2, 9);
    f1.Add(12, 4, 4);
    f1.Add(6, 15, 2);
    f1.Add(6, 9, 3);
    res = f1.Remove(12, id);
    // res = true, id = 4
    assert(res);
    assert(id == 4);
    res = f1.Remove(12, id);
    // res = false, id = N/A
    assert(res == false);
    res = f1.Remove(6, id);
    // res = true, id = 15
    assert(res);
    assert(id == 15);
    res = f1.Remove(6, id);
    // res = true, id = 9
    assert(res);
    assert(id == 9);
    res = f1.Remove(6, id);
    // res = false, id = N/A
    assert(res == false);
}

void progTest2() {
    //Ukazkovy vstup #2
//-----------------
    CHolding f2;
    f2.Add(4, 2, 2);
    f2.Add(1, 4, 3);
    f2.Add(8, 9, 8);
    res = f2.Remove(id);
    // res = true, id = 2
    assert(res);
    assert(id == 2);
    res = f2.Remove(id);
    // res = true, id = 4
    assert(res);
    assert(id == 4);


//Ukazkovy vstup #3
//-----------------
    CHolding f3;
    f3.Add(10, 101, 9);
    f3.Add(10, 102, 8);
    f3.Add(10, 103, 7);
    f3.Add(10, 104, 6);
    f3.Add(10, 105, 5);
    f3.Add(20, 201, 9);
    f3.Add(20, 202, 8);
    f3.Add(20, 203, 7);
    f3.Add(20, 204, 6);
    f3.Add(20, 205, 5);
    f3.Add(30, 301, 9);
    f3.Add(30, 302, 8);
    f3.Add(30, 303, 7);
    f3.Add(30, 304, 6);
    f3.Add(30, 305, 5);
    res = f3.Remove(id);
    // res = true, id = 105
    assert(res);
    assert(id == 105);
    res = f3.Remove(id);
    // res = true, id = 205
    assert(res);
    assert(id == 205);
    res = f3.Remove(id);
    // res = true, id = 305
    assert(res);
    assert(id == 305);
    res = f3.Remove(id);
    // res = true, id = 104
    assert(res);
    assert(id == 104);
    res = f3.Remove(id);
    // res = true, id = 204
    assert(res);
    assert(id == 204);
    res = f3.Remove(id);
    // res = true, id = 304
    assert(res);
    assert(id == 304);
    res = f3.Remove(id);
    // res = true, id = 103
    assert(res);
    assert(id == 103);
    res = f3.Remove(id);
    // res = true, id = 203
    assert(res);
    assert(id == 203);
    res = f3.Remove(id);
    // res = true, id = 303
    assert(res);
    assert(id == 303);
    res = f3.Remove(id);
    // res = true, id = 102
    assert(res);
    assert(id == 102);
    res = f3.Remove(id);
    // res = true, id = 202
    assert(res);
    assert(id == 202);
    res = f3.Remove(id);
    // res = true, id = 302
    assert(res);
    assert(id == 302);
}

void progTest3() {
    //Ukazkovy vstup #4
//-----------------
    CHolding f4;
    f4.Add(10, 101, 9);
    f4.Add(10, 102, 8);
    f4.Add(10, 103, 7);
    f4.Add(10, 104, 6);
    f4.Add(10, 105, 5); //
    f4.Add(20, 201, 9);
    f4.Add(20, 202, 8);
    f4.Add(20, 203, 7);
    f4.Add(20, 204, 6);
    f4.Add(20, 205, 5); //
    f4.Add(30, 301, 9);
    f4.Add(30, 302, 8);
    f4.Add(30, 303, 7);
    f4.Add(30, 304, 6);
    f4.Add(30, 305, 5); //

    res = f4.Remove(30, id);
    // res = true, id = 305
    assert(res);
    assert(id == 305);
    res = f4.Remove(20, id);
    // res = true, id = 205
    assert(res);
    assert(id == 205);
    res = f4.Remove(10, id);
    // res = true, id = 105
    assert(res);
    assert(id == 105);
    f4.Merge(30, 10);
    res = f4.Remove(10, id);
    // res = false, id = N/A
    assert(res == false);

    /*
    f4.Add(30, 101, 9); !
    f4.Add(30, 102, 8); !
    f4.Add(30, 103, 7); !
    f4.Add(30, 104, 6); !
    f4.Add(30, 301, 9); !
    f4.Add(30, 302, 8); !
    f4.Add(30, 303, 7); !
    f4.Add(30, 304, 6); !

    f4.Add(20, 201, 9); !
    f4.Add(20, 202, 8); !
    f4.Add(20, 203, 7); !
    f4.Add(20, 204, 6); !
     */
    res = f4.Remove(20, id);
    // res = true, id = 204
    assert(res);
    assert(id == 204);
    res = f4.Remove(30, id);
    // res = true, id = 104
    assert(res);
    assert(id == 104);
    res = f4.Remove(id);
    // res = true, id = 304
    assert(res);
    assert(id == 304);
    res = f4.Remove(id);
    // res = true, id = 203
    assert(res);
    assert(id == 203);
    res = f4.Remove(id);
    // res = true, id = 103
    assert(res);
    assert(id == 103);
    res = f4.Remove(id);
    // res = true, id = 303
    assert(res);
    assert(id == 303);
    res = f4.Remove(id);
    // res = true, id = 202
    assert(res);
    assert(id == 202);
    res = f4.Remove(id);
    // res = true, id = 102
    assert(res);
    assert(id == 102);
    res = f4.Remove(id);
    // res = true, id = 302
    assert(res);
    assert(id == 302);
    res = f4.Remove(id);
    // res = true, id = 201
    assert(res);
    assert(id == 201);
    res = f4.Remove(id);
    // res = true, id = 101
    assert(res);
    assert(id == 101);
    res = f4.Remove(id);
    // res = true, id = 301
    assert(res);
    assert(id == 301);
    res = f4.Remove(id);
    // res = false, id = N/A
    assert(res == false);
    res = f4.Remove(id);
    // res = false, id = N/A
    assert(res == false);
}

void progTest4() {
    //Ukazkovy vstup #5
//-----------------
    CHolding f5;
    f5.Add(10, 333, 5);
    f5.Add(20, 444, 2);
    f5.Add(10, 222, 6);
    f5.Add(20, 555, 8);
    res = f5.Remove(10, id);
    // res = true, id = 333
    assert(res);
    assert(id == 333);
    res = f5.Remove(id);
    // res = true, id = 444
    assert(res);
    assert(id == 444);
    f5.Merge(20, 10);
    res = f5.Remove(10, id);
    // res = false, id = N/A
    assert(res == false);
    res = f5.Remove(20, id);
    // res = true, id = 222
    assert(res);
    assert(id == 222);


//Ukazkovy vstup #6
//-----------------
    CHolding f6;
    f6.Add(10, 1, 7);
    f6.Add(20, 1, 7);
    f6.Add(30, 1, 7);
    res = f6.Merge();
    // res = true
    assert(res);
    res = f6.Remove(20, id);
    // res = false, id = N/A
    assert(res == false);
    res = f6.Remove(30, id);
    // res = true, id = 1
    assert(res);
    assert(id == 1);
    res = f6.Remove(30, id);
    // res = false, id = N/A
    assert(res == false);
    res = f6.Remove(10, id);
    // res = true, id = 1
    assert(res);
    assert(id == 1);
    res = f6.Remove(10, id);
    // res = true, id = 1
    assert(res);
    assert(id == 1);
    res = f6.Remove(10, id);
    // res = false, id = N/A
    assert(res == false);


//Ukazkovy vstup #7
//-----------------
    CHolding f7;
    f7.Add(1, 1, 1);
    f7.Add(2, 2, 1);
    f7.Add(3, 3, 1);
    res = f7.Merge();
    // res = true
    assert(res);
    res = f7.Merge();
    // res = true
    assert(res);
    res = f7.Merge();
    // res = false
    assert(res == false);
    res = f7.Remove(id);
    // res = true, id = 1
    assert(res);
    assert(id == 1);
    res = f7.Remove(id);
    // res = true, id = 2
    assert(res);
    assert(id == 2);
    res = f7.Remove(id);
    // res = true, id = 3
    assert(res);
    assert(id == 3);
    res = f7.Remove(id);
    // res = false, id = N/A
    assert(res == false);

    CHolding fX;
    int a, b, c;
    for (int i = 1; i != 100; ++i)
    {
        for (int j = 1; j != 50; ++j)
        {
            a = (rand() * 1.0 / RAND_MAX) * 100;
            b = (rand() * 1.0 / RAND_MAX) * 20000;
            c = (rand() * 1.0 / RAND_MAX) * 20000;
            fX.Add(a, b, c);
        }

        a = (rand() * 1.0 / RAND_MAX) * 10000;
        fX.Remove(a, id);

        fX.Merge();
        fX.Remove(id);

        a = (rand() * 1.0 / RAND_MAX) * 10000;
        b = (rand() * 1.0 / RAND_MAX) * 10000;
        fX.Merge(a, b);
    }
}


#endif
