#include <iostream>
#include <fstream> // std::ifstream
#include <vector>
#include <queue>
#include <algorithm>
#include <chrono>
#include <cmath>
#include "common.cpp"
#include "loaders.cpp"
#include "checkers.cpp"
#include <omp.h>

using namespace std;

int nodeCount = 0;


Result currSolution{};
int bestWeight = 0;
uint callCounter = 0;

void findSolutions(Edge usedEdge, int8_t variant, vector<Node> nodes, Edges edges, Edges acc) {
    ++callCounter;
    if (currSolution.weight > edges.weight + acc.weight)
        return;

    if (!setColor(variant, usedEdge.nodes, nodes))
        return;

    if (edges.data.empty()) {
        if (!isCoherent(acc, nodes.size()))
            return;

        Result result = {.weight=acc.weight, .data={acc.data}};
        updateBest(currSolution, result, bestWeight);

        return;
    }

    Edge edge = pop(edges);
    acc = push(edge, acc);

    findSolutions(edge, RED, nodes, edges, acc);
    findSolutions(edge, BLACK, nodes, edges, acc);

    pop(acc);
    findSolutions(Edge{}, NONE, nodes, edges, acc);
}

Result startSearch(vector<Node> &nodes, Edges &edges) {
    if (nodes.empty()) return {};

    int numOfProblems = 1;
    #pragma omp parallel shared(numOfProblems)
    {
        #pragma omp single
        numOfProblems = omp_get_num_threads() * 70;
    }


    Partial job{Edge{}, NONE, nodes, edges, Edges{}};
    deque<Partial> q = generateProblems(job, numOfProblems);
    cout << "problems generated " << q.size() << endl;

    #pragma omp parallel for schedule(dynamic ) shared(currSolution)
    for (int i = 0; i < q.size(); i++) {
        auto &partial = q[i];
        findSolutions(partial.edge, partial.variant, partial.nodes, partial.edges, partial.acc);
    }

    return currSolution;
}

int main(int argc, char *argv[]) {
    auto start = getTime();
    
    if (argc < 2) {
        cerr << "Missing argument: provide path to input file";
        return 1;
    } else if (argc > 2) {
        cerr << "Too many arguments";
        return 1;
    }
    char *file_path = argv[1];

    vector<int> matrix = loadMatrix(file_path, nodeCount);
    Edges edges = loadEdges(matrix, nodeCount);
    sort(edges.data.begin(), edges.data.end(), cmpDescending);

    vector<Node> nodes(nodeCount);
    auto neighbours = loadNeighbours(matrix, nodeCount);
    bool bipartite = isBipartite(nodes, neighbours);
    cout << "isBipartite=" << bipartite << endl;
    
    #pragma omp parallel
    {
        #pragma omp single
        cout << "number of threads: " << omp_get_num_threads() << endl; 
    }

    nodes[0].color = BLACK;
    Result results = startSearch(nodes, edges);
    printResults(results);

    auto end = getTime();

    cout << endl << "Measurements:" << endl;
    cout << "called: " << callCounter << endl;
    cout << "passed: " << end - start << " ms" << endl;
    cout << "passed: " << (end - start) / 1000 << " s" << endl;

    return 0;
}