#include <vector>
#include <iostream>
#include <fstream>
#include "common.h"

using namespace std;

void addWeight(int weight, int i, int j, vector<int> &matrix, int nodeCount) {
    matrix[i * nodeCount + j] = weight;
}

int getWeight(int i, int j, vector<int> &matrix, int nodeCount) {
    return matrix[i * nodeCount + j];
}

vector<int> loadMatrix(char *file_path, int &nodeCount) {
    cout << "Loading file: " << file_path << endl;

    ifstream myFile;
    myFile.open(file_path, ifstream::in);

    if (myFile.fail()) {
        throw invalid_argument("file not found");
    }

    myFile >> nodeCount;

    int matrixSize = nodeCount * nodeCount;
    vector<int> matrix(matrixSize);

    int weight = 0;
    for (int i = 0; i < nodeCount; ++i) {
        for (int j = 0; j < nodeCount; ++j) {
            myFile >> weight;
            addWeight(weight, i, j, matrix, nodeCount);
        }
    }

    return matrix;
}

Edges loadEdges(vector<int> &matrix, int nodeCount) {
    Edges edges;
    for (int i = 0; i < nodeCount; ++i) {
        for (int j = 0; j < i; ++j) {
            int weight = getWeight(i, j, matrix, nodeCount);
            if (weight != 0) {
                Edge edge = {.weight = weight, .nodes = {i, j}};
                edges.data.push_back(edge);
                edges.weight += edge.weight;
            }
        }
    }
    return edges;
}

vector<vector<int> > loadNeighbours(vector<int> matrix, int nodeCount) {
    vector<vector<int> > neighbours(nodeCount);

    for (int i = 0; i < nodeCount; ++i) {
        for (int j = 0; j < nodeCount; ++j) {
            if (getWeight(i, j, matrix, nodeCount) != 0) {
                neighbours[i].push_back(j);
            }
        }
    }

    return neighbours;
}
