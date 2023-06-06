#include <vector>
#include <iostream>
#include <chrono>
#include <queue>
#include "common.h"
#include "utils.cpp"

bool bipartiteRec(int nodeId, vector<Node> &nodes, vector<vector<int> > &neighbours) {
    const Node current = nodes[nodeId];
    for (int next: neighbours[nodeId]) {
        if (nodes[next].color == NONE) {
            nodes[next].color = invertColor(current.color);
            if (!bipartiteRec(next, nodes, neighbours)) {
                return false;
            }
        } else if (nodes[next].color == invertColor(current.color)) {
            // pass
        } else if (nodes[next].color == current.color) {
            return false;
        }
    }

    return all_of(nodes.begin(), nodes.end(), hasColor);
}

bool isBipartite(vector<Node> nodes, vector<vector<int>> neighbours) {
    nodes[0].color = RED;
    return bipartiteRec(0, nodes, neighbours);
}

bool isCoherent(Edges edgesObj, int nodeCount) {
    vector<Edge> &edges = edgesObj.data;
    if (edges.empty()) return false;

    vector<Node> nodes(nodeCount);
    queue<int> q;
    Edge edge = edges.back();
    edges.pop_back();

    pushAndMark(edge.nodes.first, q, nodes);
    pushAndMark(edge.nodes.second, q, nodes);

    while (!q.empty()) {
        auto nodeId = q.front();
        q.pop();

        for (auto it = edges.begin(); it != edges.end();) {
            int id1 = it->nodes.first;
            int id2 = it->nodes.second;

            if (id1 == nodeId || id2 == nodeId) {
                it = edges.erase(it);
                pushAndMark(id1, q, nodes);
                pushAndMark(id2, q, nodes);
            } else {
                ++it;
            }
        }
    }

    return edges.empty();
}