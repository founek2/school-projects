#include <vector>
#include "common.h"


using namespace std;

Result &concat(Result &a, Result &b) {
    a.data.insert(a.data.end(), b.data.begin(), b.data.end());
    return a;
}


Result mergeSolutions(Result &r1, Result &r2, Result &r3) {
    Result final;
    int maxWeight = max({r1.weight, r2.weight, r3.weight});

    final.weight = maxWeight;
    if (r1.weight == maxWeight)
        final = concat(final, r1);

    if (r2.weight == maxWeight)
        final = concat(final, r2);

    if (r3.weight == maxWeight)
        final = concat(final, r3);

    return final;
}

Result mergeAll(vector<Result> &solutions) {
    if (solutions.empty())
        return {};

    auto it = std::max_element(solutions.begin(),
                               solutions.end(),
                               [](const Result& a,const Result& b) { return a.weight < b.weight; });


    Result final;
    final.weight = it->weight;
    for (auto & solution : solutions) {
        if (final.weight == solution.weight) {
            final = concat(final, solution);
        }
    }

    return final;
}

