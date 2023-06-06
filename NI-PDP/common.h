//
// Created by Martin Skalický on 14.03.2022.
//
#include <boost/serialization/access.hpp>
#include <boost/serialization/vector.hpp>

#ifndef UNTITLED_COMMON_H
#define UNTITLED_COMMON_H

using namespace std;

const int8_t BLACK = 0;
const int8_t RED = 1;
const int8_t NONE = -1;

struct IDs {
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /* file_version */){
        ar & first & second;
    }

    int first;
    int second;
};

struct Edge {
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /* file_version */){
        ar & weight & nodes;
    }

    int weight = 0;
    IDs nodes;
};

struct Edges {
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /* file_version */){
        ar & weight & data;
    }

    int weight = 0;
    vector<Edge> data;
};

struct Result {
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /* file_version */){
        ar & weight & data;
    }

    int weight = 0;
    vector<vector<Edge> > data = {};
};

struct Node {
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /* file_version */){
        ar & color;
    }

    int8_t color = NONE;
};


struct Partial {
    friend class boost::serialization::access;
    template<class Archive>
    void serialize(Archive & ar, const unsigned int /* file_version */){
        ar & edge & variant & nodes & edges & acc & bestWeight;
    }

    Edge edge;
    int8_t variant;
    vector<Node> nodes;
    Edges edges;
    Edges acc;
    int bestWeight = 0;
};

int DONE_TAG = 1;
int JOB_TAG = 2;
int MAX_TAG = 3;
int EXIT_TAG = 4;
#endif //UNTITLED_COMMON_H
