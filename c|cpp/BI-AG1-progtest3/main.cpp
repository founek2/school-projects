#include <iostream>
#include <vector>
#include <climits>
#include <algorithm>

struct Mem {
    long cost;
    int num;
};

using Cache = std::vector<std::vector<Mem >>;

struct Point {
    int liters;
    std::vector<unsigned int> neighbours;
    int bear_num = -1;
};

struct Bear {
    int num;
    long price;

    bool operator<(const Bear &other) const {
        return price < other.price;
    }
};

void print_backtrack(Cache &cache, std::vector<Bear> &bears, std::vector<Point> & restaurants);
void print_cache(Cache &cache, std::vector<Bear> &bears);
void calculate_optimal_bears(unsigned curr_i, int ancestor_i, int ancestor_bear,  Cache &cache,  std::vector<Point> &restaurants,
                             unsigned bears_size);

unsigned get_max_bear_try(unsigned bears_size, unsigned neighbours_size) {
    return bears_size <= neighbours_size + 1 ? bears_size : neighbours_size + 1;
}

/**
 *
 * @param curr_i - current index of restaurant
 * @param bear_i - index of bear, that cannot be used
 * @param ancestor_i - index of ancestor
 * @param bears - vector of bears
 * @param restaurants - vector<vector<Point>> of restaurants
 * @param cache - cache for memoize
 * @return minimal price
 */
long find_min_price(int curr_i, int ancestor_bear_i, int ancestor_i, std::vector<Bear> &bears,
                   std::vector<Point> &restaurants,
                   Cache &cache) {
    if (ancestor_bear_i == -1 || cache[curr_i][ancestor_bear_i].cost == -1) {  // need calculate price
        Point *curr = &(restaurants[curr_i]);
        unsigned neighbour_size = curr->neighbours.size();
        //unsigned max_bear_try = bears.size() <= neighbour_size + 1 ? bears.size() : neighbour_size + 1;
        unsigned max_bear_try = get_max_bear_try(bears.size(), neighbour_size);


        Mem best_mem = {LONG_MAX, -1};
        for (unsigned bear_i = 0; bear_i < max_bear_try; ++bear_i) {
            if ((int) bear_i != ancestor_bear_i) {
                long price_neighbours = 0;
                for (unsigned j = 0; j < neighbour_size; ++j) {
                    int neighbour_i = curr->neighbours[j];
                    long price_neighbour = LONG_MAX;
                    if (neighbour_i != ancestor_i) {

                        long price = find_min_price(neighbour_i, (int) bear_i, curr_i, bears, restaurants, cache);
                        price_neighbour = price < price_neighbour ? price : price_neighbour;

                        price_neighbours += price_neighbour;
                    }
                }
                long final_price = price_neighbours + restaurants[curr_i].liters * bears[bear_i].price;
                if (best_mem.cost > final_price ) {
                    best_mem =  {final_price, (int) bear_i};
                }
            }
        }

        if (ancestor_bear_i > -1) {
            Mem *my_cache = &(cache[curr_i][ancestor_bear_i]);
            if (my_cache->cost == -1 || my_cache->cost > best_mem.cost){
                *my_cache = best_mem;
            }
        } else return best_mem.cost;
    }

    return cache[curr_i][ancestor_bear_i].cost;
}

int main() {
    int point_count = 0, bears = 0;
    std::cin >> point_count >> bears;

    std::vector<Bear> bear_prices(bears);
    std::vector<Point> restaurants(point_count);

    // load bear prices
    for (int i = 0; i < bears; ++i) {
        std::cin >> bear_prices[i].price;
        bear_prices[i].num = i + 1;
    }

    std::sort(bear_prices.begin(), bear_prices.end());

    // load liters for each restaurant
    for (int i = 0; i < point_count; ++i) {
        std::cin >> restaurants[i].liters;
    }

    // bidirectional edges
    for (int i = 0; i < point_count - 1; ++i) {
        unsigned int from, to;
        std::cin >> from >> to;

        restaurants[from - 1].neighbours.push_back(to - 1);
        restaurants[to - 1].neighbours.push_back(from - 1);
    }

    Cache cache(point_count, std::vector<Mem>(bears, {-1, -1}));

    long min_price = find_min_price(0, -1, -1, bear_prices, restaurants, cache);

    std::cout << min_price << std::endl;
    if (restaurants.size() > 1)
        print_backtrack(cache, bear_prices, restaurants);
    else
        std::cout << min_price << std::endl;
    // print_cache(cache, bear_prices);

    return 0;
}

void print_backtrack(Cache &cache, std::vector<Bear> &bears, std::vector<Point> &restaurants) {
    int last_ancestor_bear = 0;
    Mem *best_mem = &(cache[0][last_ancestor_bear]);
    *best_mem = {LONG_MAX, -1};
    Point first_point = restaurants[0];
    int liters = first_point.liters;

    unsigned bears_max_try = get_max_bear_try(bears.size(), first_point.neighbours.size());
    for (unsigned bear_i = 0; bear_i < bears_max_try; ++bear_i) {    // calculate bear for zero restaurant
        long price = 0;
        for (unsigned i = 0; i < first_point.neighbours.size(); ++i) {
            int neighbour_i = first_point.neighbours[i];
            if (cache[neighbour_i][bear_i].cost != -1)
                price += cache[neighbour_i][bear_i].cost;
        }
        price += liters * bears[bear_i].price;
        if (best_mem->cost > price )
            *best_mem = {price, (int) bear_i};
    }

    calculate_optimal_bears(0, -1, last_ancestor_bear, cache, restaurants, bears.size());

    for (unsigned i = 0; i < restaurants.size(); ++i) {    // access to cache and print bear
        int bear_i = restaurants[i].bear_num;

        std::cout << bears[bear_i].num;

        if (i != cache.size() - 1)
            std::cout << " ";
    }
    std::cout << std::endl;
}

void calculate_optimal_bears(unsigned curr_i, int ancestor_i, int ancestor_bear,  Cache &cache,  std::vector<Point> &restaurants,
                             unsigned bears_size) {
    int my_best_bear = cache[curr_i][ancestor_bear].num;
    restaurants[curr_i].bear_num = my_best_bear;

    for (unsigned i = 0; i < restaurants[curr_i].neighbours.size(); ++i) {
        int neighbour_i = restaurants[curr_i].neighbours[i];
        if (neighbour_i != ancestor_i)
            calculate_optimal_bears(neighbour_i, (int) curr_i, my_best_bear, cache, restaurants, bears_size);
    }
}

void print_cache(Cache &cache, std::vector<Bear> &bears) {
    for (unsigned i = 0; i < cache.size(); ++i) {
        std::cout << i << ":";
        for (unsigned j = 0; j < cache[i].size(); ++j) {
            int bear_num = cache[i][j].num != -1 ? bears[cache[i][j].num].num : -1;
            std::cout << " " << cache[i][j].cost << "," << bear_num;
        }
        std::cout << std::endl;
    }
}