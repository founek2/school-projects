#include <iostream>
#include <vector>
#include <algorithm>
#include <chrono>

#include "common.cpp"
#include "loaders.cpp"
#include "checkers.cpp"
#include <omp.h>

using namespace std;

#define TASK_SIZE 12

int nodeCount = 0;

Result currSolution{};
int bestWeight = 0;
uint callCounter = 0;

void findSolutions(Edge usedEdge, int8_t variant, vector<Node> nodes, Edges edges, Edges acc) {
    ++callCounter;
    if (bestWeight > edges.weight + acc.weight)
        return;


    if (!setColor(variant, usedEdge.nodes, nodes))
        return;


    if (edges.data.empty()) {
        if (!isCoherent(acc, nodeCount))
            return;

        Result result = {.weight=acc.weight, .data={acc.data}};
        updateBest(currSolution, result, bestWeight);
        return;
    }

    Edge edge = pop(edges);

    acc = push(edge, acc);

#pragma omp task shared(bestWeight, callCounter, currSolution) if (edges.data.size() >TASK_SIZE)
    findSolutions(edge, RED, nodes, edges, acc);
#pragma omp task  shared(bestWeight, callCounter, currSolution) if (edges.data.size() >TASK_SIZE)
    findSolutions(edge, BLACK, nodes, edges, acc);

    pop(acc);
#pragma omp task  shared(bestWeight, callCounter, currSolution) if (edges.data.size() >TASK_SIZE)
    findSolutions(Edge{}, NONE, nodes, edges, acc);
}

Result startSearch(vector<Node> &nodes, Edges &edges, Edges &acc) {
    if (nodes.empty()) return {};
    nodes[0].color = BLACK;

#pragma omp parallel default(shared)
    {
#pragma omp single
        findSolutions(Edge{}, NONE, nodes, edges, acc);
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

    //printEdges(edges);

    vector<Node> nodes(nodeCount);

    auto neighbours = loadNeighbours(matrix, nodeCount);
    bool bipartite = isBipartite(nodes, neighbours);
    cout << "isBipartite=" << bipartite << endl;

    #pragma omp parallel
    {
        #pragma omp single
        cout << "number of threads: " << omp_get_num_threads() << endl; 
    }


    Edges acc;
    Result results = startSearch(nodes, edges, acc);
    printResults(results);

    auto end = getTime();

    cout << endl << "Measurements:" << endl;
    cout << "called: " << callCounter << endl;
    cout << "passed: " << end - start << " ms" << endl;
    cout << "passed: " << (end - start) / 1000 << " s" << endl;

    return 0;
}