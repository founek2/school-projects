#include <boost/mpi/environment.hpp>
#include <boost/mpi/communicator.hpp>
#include <boost/serialization/utility.hpp>
#include <iostream>
#include <vector>
#include <boost/mpi.hpp>
#include <boost/serialization/vector.hpp>
#include "common.h"

namespace mpi = boost::mpi;


int main()
{
    mpi::environment env;
    mpi::communicator world;
    std::cout << "I am process " << world.rank() << " of " << world.size()
              << "." << std::endl;

    Edges value;
    value.weight = 11;
    value.data.push_back(11);
    value.data.push_back(12);
    value.data.push_back(13);

    if (world.rank() == 0) {
        int job_id = 0;
        std::vector<mpi::request> reqs(world.size());
        for (int dst_rank = 1; dst_rank < world.size(); ++dst_rank) {
            std::cout << "[MASTER] Sending job " << job_id
                      << " to SLAVE " <<  dst_rank << "\n";
            // Send job to dst_rank [nonblocking]
            world.send(dst_rank, job_id, value);
            // Post receive request for new jobs requests by slave [nonblocking]
            //reqs[dst_rank] = world.irecv(dst_rank, 0);
            ++job_id;
        }
        world.send(1,  0, value);
        //mpi::broadcast(world, tag, value);
    }else {
        Edges msg;
        boost::mpi::status s = world.recv(mpi::any_source, mpi::any_tag, msg);
        std::cout << msg.weight << ", from " <<s.source() <<std::endl;
    }
    return 0;
}