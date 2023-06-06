#include <vector>
#include <iostream>
#include <chrono>
#include <queue>
#include "common.h"
#include <cmath>

int8_t invertColor(int8_t color) {
    if (color == BLACK)
        return RED;
    if (color == RED)
        return BLACK;

    cout << "warning, not inverting NONE" << endl;
    return NONE;
}

bool hasColor(Node &node) {
    return node.color != NONE;
}


bool setColor1(int8_t color, int id, vector<Node> &nodes) {
    if (nodes[id].color == color || nodes[id].color == NONE) {
        nodes[id].color = color;
        return true;
    }

    return false;
}

bool setColor(int8_t variant, IDs ids, vector<Node> &nodes) {
    if (variant == NONE) return true;

    int8_t color1 = variant == BLACK ? BLACK : RED;
    int8_t color2 = variant == BLACK ? RED : BLACK;

    return setColor1(color1, ids.first, nodes) && setColor1(color2, ids.second, nodes);
}

Edge pop(Edges &edges) {
    Edge edge = edges.data.back();
    edges.data.pop_back();
    edges.weight -= edge.weight;

    return edge;
}

Edges &push(Edge edge, Edges &edges) {
    edges.data.push_back(edge);
    edges.weight += edge.weight;
    return edges;
}


void pushAndMark(int nodeId, queue<int> &q, vector<Node> &nodes) {
    if (nodes[nodeId].color != NONE) return;

    q.push(nodeId);
    setColor1(RED, nodeId, nodes);
}

long getTime() {
    return chrono::duration_cast<chrono::milliseconds>(chrono::system_clock::now().time_since_epoch()).count();
}

bool cmpDescending(const Edge &a, const Edge &b) { return (a.weight > b.weight); }


void printNeighbours(const vector<vector<int> > &neighbours) {
    cout << "Neighbours" << endl;
    for (int i = 0; i < neighbours.size(); ++i) {
        cout << i << ":";
        for (auto nodeId: neighbours[i]) {
            cout << " " << nodeId;
        }
        cout << endl;
    }
}

void printEdges(const Edges &edges) {
    cout << "Edges weight=" << edges.weight << endl;
    for (int i = 0; i < edges.data.size(); ++i) {
        cout << i << ":";

        cout << " from " << edges.data[i].nodes.first;
        cout << ", to " << edges.data[i].nodes.second;

        cout << endl;
    }
}

void printNodes(const vector<Node> nodes) {
    cout << "Nodes: ";
    for (int i = 0; i < nodes.size(); ++i) {
        cout << std::to_string(nodes[i].color) <<", ";
    }
    cout << endl;
}


void printResults(const Result &results) {
    cout << "Results:" << endl;

    for (const auto &result: results.data) {
        cout << "weight=" << results.weight << endl;
        for (auto edge: result) {
            cout << edge.nodes.first << ":" << edge.nodes.first << ", ";
        }
        cout << endl;
    }
}



deque<Partial> generateProblems(Partial & job, int numOfProblems) {
    if (job.nodes.empty()) return {};

    deque<Partial> q{job};
    //cout << "Gen> " << std::to_string(variant) << " " << e.nodes.first << "," << e.nodes.second << " nodes=" << nodes.size() << endl;
    //q.push_back({e, variant, nodes, edges, acc});

    while (q.size() < numOfProblems && !q.empty()) {
        auto partial = q.front();
        q.pop_front();
        if (!setColor(partial.variant, partial.edge.nodes, partial.nodes)) {
            continue;
        }

        Edge edge = pop(partial.edges);
        partial.acc = push(edge, partial.acc);

        q.push_back({edge, RED, partial.nodes, partial.edges, partial.acc});
        q.push_back({edge, BLACK, partial.nodes, partial.edges, partial.acc});

        pop(partial.acc);
        q.push_back({edge, NONE, partial.nodes, partial.edges, partial.acc});
    }
    return q;
}

void updateBest(Result& current, Result newBest, int &bestWeight) {
#pragma omp critical(best)
    {
        int maxWeight = max({current.weight, newBest.weight});
        if (maxWeight >= bestWeight){
            if (newBest.weight > current.weight) {
                current = newBest;
                bestWeight = newBest.weight;
            } else if (current.weight == newBest.weight) {
                current = concat(current, newBest);
            }
        }
    }
}