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
#include <boost/mpi/environment.hpp>
#include <boost/mpi/communicator.hpp>
#include <unistd.h>

namespace mpi = boost::mpi;
using namespace std;

int nodeCount = 0;

uint callCounter = 0;
int bestWeight = 0;
Result currSolution{};

void findSolutions(Edge usedEdge, int8_t variant, vector<Node> nodes, Edges edges, Edges acc) {
    ++callCounter;
#pragma atomic read
    if (bestWeight > edges.weight + acc.weight)
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

void BroadcastWeight(mpi::communicator &world, int weight, int sourceRank) {
    for (unsigned int dst_rank = 1; dst_rank < world.size(); ++dst_rank) {
        if (sourceRank != dst_rank) world.send(dst_rank, MAX_TAG, weight);
    }
}

Result startSearchAndSend(vector<Node> &nodes, Edges &edges, mpi::environment &env, mpi::communicator &world) {
    if (nodes.empty()) return {};

    Edges acc;
    int numOfProblems = world.size() * 70;
    Partial job{Edge{}, NONE, nodes, edges, acc, currSolution.weight};
    deque<Partial> q = generateProblems(job, numOfProblems);
    std::cout << "[MASTER] generated problems: " << q.size() << endl;

    // Send initial jobs
    for (unsigned int dst_rank = 1; dst_rank < world.size(); ++dst_rank) {
        //std::cout << "[MASTER] Sending job " << dst_rank - 1
        //          << " to SLAVE " <<  dst_rank << "\n";

        auto problem = q.front();
        q.pop_front();

        world.send(dst_rank, JOB_TAG, problem);
    }

    std::cout << "[MASTER] all jobs assigned" << endl;

    boost::mpi::status s = world.recv(mpi::any_source, DONE_TAG, currSolution);
    while (!q.empty()) {
        auto problem = q.front();
        q.pop_front();
        problem.bestWeight = currSolution.weight;
        world.send(s.source(), JOB_TAG, problem);

        Result newResult;
        boost::mpi::status s = world.recv(mpi::any_source, DONE_TAG, newResult);
        //std::cout << "[MASTER] got result, weight=" <<newResult.weight << " vs " << best.weight <<endl;

        if (newResult.weight == currSolution.weight) {
            currSolution = concat(currSolution, newResult);
        } else if (newResult.weight > currSolution.weight) {
            currSolution = newResult;
            BroadcastWeight(world, currSolution.weight, s.source());
        }

        cout << "." << flush;
    }
    cout << endl;

    Partial emptyJob = {.bestWeight=-1};
    for (unsigned int dst_rank = 1; dst_rank < world.size(); ++dst_rank) {
        world.send(dst_rank, JOB_TAG, emptyJob);
        world.send(dst_rank, MAX_TAG, -1);
    }

    cout << "MASTER exiting..." << endl;
    return currSolution;
}


Result startSearch(Partial &job, mpi::communicator &world) {
    int numOfProblems = 1;
    #pragma omp parallel shared(numOfProblems)
    {
        #pragma omp single
        numOfProblems = omp_get_num_threads() * 70;
    }

    deque<Partial> q = generateProblems(job, numOfProblems);

    #pragma omp parallel for schedule(dynamic)
    for (int i = 0; i < q.size(); i++) {
        //cout << "processing item " << i << endl;
        auto &partial = q[i];
        //cout << "partial " << std::to_string(partial.variant) << endl;
        findSolutions(partial.edge, partial.variant, partial.nodes, partial.edges, partial.acc);
    }

    return currSolution;
}

int main(int argc, char *argv[]) {
    auto start = getTime();

    mpi::environment env(mpi::threading::level::multiple);
    mpi::communicator world;

    omp_set_nested(1);

    /********************************* MASTER **********************************/
    if (world.rank() == 0) {
        cout << "Processes: " << world.size() << endl;
        cout << "thread level: " << env.thread_level() << endl;
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

        nodes[0].color = BLACK;
        Result results = startSearchAndSend(nodes, edges, env, world);
        //printResults(results);


        auto end = getTime();

        cout << endl << "Measurements:" << endl;
        cout << "weight " << results.weight << endl;
        cout << "called: " << callCounter << endl;
        cout << "passed: " << end - start << " ms" << endl;
        cout << "passed: " << (end - start) / 1000 << " s" << endl;
    }

    /********************************* SLAVES **********************************/
    if (world.rank() != 0) {
#pragma omp parallel sections
        {
#pragma omp section
            {
                cout << "[SLAVE" << world.rank() << "] Threads: " << omp_get_num_threads() << endl;

                while (omp_get_num_threads() > 1) {
                    int newBestWeight;
                    world.recv(mpi::any_source, MAX_TAG, newBestWeight);
                    if (newBestWeight == -1) break;
                    
                    Result newBest = {.weight=newBestWeight};
                    updateBest(currSolution, newBest, bestWeight);
                }

            }

#pragma omp section
            {
                while (true) {
                    Partial job;
                    world.recv(mpi::any_source, JOB_TAG, job);
                    if (job.bestWeight == -1) break;

                    Result solution = startSearch(job, world);
                    world.send(0, DONE_TAG, solution);
                }

            }
        }
    }


    cout << "[SLAVE" << world.rank() << "] called " << callCounter << endl;
    cout << "[SLAVE" << world.rank() << "] exiting work..." << endl;

    return 0;
}