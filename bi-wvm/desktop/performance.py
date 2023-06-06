import operator
import time
from algorithms.sweep_plane2 import sweep_plane_alg
from algorithms.brute_force2 import brute_force_alg
from algorithms.divide_and_conquer2 import divide_and_conquer_alg
import csv

items_count = 2000
x_name, y_name, order1, order2 = "Year", "Price", "Asc", "Asc"

if __name__ == "__main__":
    with open('partial1m.csv', mode='r') as infile:
        print("Loading data...")
        reader = csv.reader(infile)
        header = next(reader)
        # print(', '.join(header))

        data = []

        for row in reader:
            obj = {}
            for i in range(0, len(header)):
                try:
                    obj[header[i]] = int(row[i])
                except ValueError:
                    obj[header[i]] = row[i]
            data.append(obj)

    print("Sorting...")
    data = sorted(data, key=operator.itemgetter(x_name, y_name))
    num = 10

    print("Starting calculations...")
    for cnt in [1000, 10000, 100000, 1000000]:
    # for cnt in [100000, 1000000]:
        times = [0, 0, 0]
        skylines = [[], [], []]
        data_curr = data[0:cnt]
        for i in range(0, num):
            start = time.time()
            skylines[0] = sweep_plane_alg(data_curr, x_name, y_name, order1 == "Asc", order2 == "Asc")
            times[0] += time.time() - start
            #print("1")
            start = time.time()
            # skylines[1] = brute_force_alg(data_curr, x_name, y_name, order1 == "Asc", order2 == "Asc")
            times[1] += time.time() - start
            #print("2")
            start = time.time()
            skylines[2] = divide_and_conquer_alg(data_curr, x_name, y_name, order1 == "Asc", order2 == "Asc")
            times[2] += time.time() - start

        # assert skylines[0] == skylines[1] == skylines[2]

        print("")
        print("Items count> ", cnt)
        print("Sweep plane alg - ", times[0] /num, "sec")
        print("Brute force alg - ", times[1] /num, "sec")
        print("Divide and conc alg - ", times[2] /num, "sec")